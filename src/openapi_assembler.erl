-module(openapi_assembler).
-export([assemble/3]).

%% Assemble complete OpenAPI spec from handler routes, schemas, and metadata
%% Routes: list of #{path, method, operation_id} (currently unused, paths extracted from schemas)
%% Schemas: list of complete operation schema maps
%% Metadata: global OpenAPI metadata map
assemble(_Routes, Schemas, Metadata) ->
    %% Build paths object from schemas
    Paths = build_paths(Schemas),

    %% Start with metadata and add paths
    Metadata#{
        <<"paths">> => Paths
    }.

%% Build paths object from operation schemas
build_paths(Schemas) ->
    %% Group operations by path
    Grouped = lists:foldl(
        fun(Schema, Acc) ->
            Path = maps:get(<<"path">>, Schema),
            Method = string:lowercase(binary_to_list(maps:get(<<"method">>, Schema))),
            MethodBin = list_to_binary(Method),
            Operation = build_operation(Schema),

            %% Get existing methods for this path or create new map
            PathMethods = maps:get(Path, Acc, #{}),

            %% Add this method/operation
            NewPathMethods = PathMethods#{MethodBin => Operation},

            Acc#{Path => NewPathMethods}
        end,
        #{},
        Schemas
    ),
    Grouped.

%% Build operation object from schema
build_operation(Schema) ->
    Base = #{},

    %% Add operationId (required)
    WithOpId = case maps:get(<<"operation_id">>, Schema, undefined) of
        undefined -> Base;
        OpId -> Base#{<<"operationId">> => OpId}
    end,

    %% Add optional top-level fields
    WithSummary = add_if_present(WithOpId, <<"summary">>, Schema),
    WithDesc = add_if_present(WithSummary, <<"description">>, Schema),
    WithTags = add_if_present(WithDesc, <<"tags">>, Schema),
    WithSecurity = add_if_present(WithTags, <<"security">>, Schema),
    WithParams = add_if_present(WithSecurity, <<"parameters">>, Schema),
    WithDeprecated = add_if_present(WithParams, <<"deprecated">>, Schema),

    %% Build requestBody from request schema if present
    WithReqBody = case maps:get(<<"request">>, Schema, undefined) of
        undefined ->
            WithDeprecated;
        <<"undefined">> ->
            %% Request is the string "undefined", no request body
            WithDeprecated;
        RequestSchema when is_map(RequestSchema) ->
            %% Remove $schema field if present
            CleanRequestSchema = maps:remove(<<"$schema">>, RequestSchema),
            ReqBody = #{
                <<"required">> => true,
                <<"content">> => #{
                    <<"application/json">> => #{
                        <<"schema">> => CleanRequestSchema
                    }
                }
            },
            WithDeprecated#{<<"requestBody">> => ReqBody};
        _ ->
            %% Not a valid request schema
            WithDeprecated
    end,

    %% Build responses from responses schema
    WithResponses = case maps:get(<<"responses">>, Schema, undefined) of
        undefined ->
            %% No responses defined, add default
            WithReqBody#{<<"responses">> => #{
                <<"200">> => #{<<"description">> => <<"Success">>}
            }};
        ResponsesMap when is_map(ResponsesMap) ->
            %% Convert response schemas to OpenAPI format
            OpenApiResponses = maps:fold(
                fun(StatusCode, ResponseSchema, Acc) when is_map(ResponseSchema) ->
                    %% Remove $schema field if present
                    CleanResponseSchema = maps:remove(<<"$schema">>, ResponseSchema),
                    Response = #{
                        <<"description">> => maps:get(<<"description">>, CleanResponseSchema, <<"Success">>),
                        <<"content">> => #{
                            <<"application/json">> => #{
                                <<"schema">> => CleanResponseSchema
                            }
                        }
                    },
                    Acc#{StatusCode => Response};
                   (_StatusCode, _ResponseSchema, Acc) ->
                    %% Skip invalid response schemas
                    Acc
                end,
                #{},
                ResponsesMap
            ),
            WithReqBody#{<<"responses">> => OpenApiResponses};
        _ ->
            %% Invalid responses, add default
            WithReqBody#{<<"responses">> => #{
                <<"200">> => #{<<"description">> => <<"Success">>}
            }}
    end,

    WithResponses.

%% Add field to map if present in source
add_if_present(Map, Key, Source) ->
    case maps:get(Key, Source, undefined) of
        undefined -> Map;
        Value -> Map#{Key => Value}
    end.

