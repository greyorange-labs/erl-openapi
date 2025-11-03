-module(openapi_assembler).
-export([assemble/3, assemble/4]).

%% Assemble complete OpenAPI spec from handler routes, schemas, components, and metadata
%% Routes: list of #{path, method, operation_id} (currently unused, paths extracted from schemas)
%% Schemas: list of complete operation schema maps
%% Components: map of component schemas (SchemaName => SchemaBody)
%% Metadata: global OpenAPI metadata map
assemble(_Routes, Schemas, Components, Metadata) ->
    %% Build paths object from schemas
    Paths = build_paths(Schemas),

    %% Start with metadata and add paths
    WithPaths = Metadata#{<<"paths">> => Paths},
    
    %% Add components.schemas if any exist
    case maps:size(Components) of
        0 ->
            %% No components (inline schemas only)
            WithPaths;
        _ ->
            %% Add components section
            ExistingComponents = maps:get(<<"components">>, WithPaths, #{}),
            WithPaths#{
                <<"components">> => ExistingComponents#{
                    <<"schemas">> => Components
                }
            }
    end.

%% Legacy version for backward compatibility
assemble(Routes, Schemas, Metadata) ->
    assemble(Routes, Schemas, #{}, Metadata).

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

    %% Build requestBody from request/requestBody schema if present
    WithReqBody = case {maps:get(<<"requestBody">>, Schema, undefined), maps:get(<<"request">>, Schema, undefined)} of
        {undefined, undefined} ->
            WithDeprecated;
        {RequestBody, _} when is_map(RequestBody) ->
            %% Already has requestBody in OpenAPI format, use as-is
            WithDeprecated#{<<"requestBody">> => RequestBody};
        {undefined, <<"undefined">>} ->
            %% Request is the string "undefined", no request body
            WithDeprecated;
        {undefined, RequestSchema} when is_map(RequestSchema) ->
            %% Check if requestSchema is already in OpenAPI format
            ReqIsOpenApiFormat = maps:is_key(<<"content">>, RequestSchema) orelse 
                            maps:is_key(<<"required">>, RequestSchema),
            
            case ReqIsOpenApiFormat of
                true ->
                    %% Already in OpenAPI format
                    WithDeprecated#{<<"requestBody">> => RequestSchema};
                false ->
                    %% Bare schema, wrap in OpenAPI format
                    CleanRequestSchema = maps:remove(<<"$schema">>, RequestSchema),
                    ReqBody = #{
                        <<"required">> => true,
                        <<"content">> => #{
                            <<"application/json">> => #{
                                <<"schema">> => CleanRequestSchema
                            }
                        }
                    },
                    WithDeprecated#{<<"requestBody">> => ReqBody}
            end;
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
            %% Check if responses are already in OpenAPI format (have 'content' or 'description')
            %% or if they need to be wrapped
            OpenApiResponses = maps:fold(
                fun(StatusCode, ResponseSchema, Acc) when is_map(ResponseSchema) ->
                    %% Check if already in OpenAPI format (has 'content' or 'description' at top level)
                    RespIsOpenApiFormat = maps:is_key(<<"content">>, ResponseSchema) orelse 
                                         maps:is_key(<<"description">>, ResponseSchema),
                    
                    Response = case RespIsOpenApiFormat of
                        true ->
                            %% Already in OpenAPI format, use as-is (just remove $schema if present)
                            maps:remove(<<"$schema">>, ResponseSchema);
                        false ->
                            %% Bare schema, wrap in OpenAPI format
                            CleanResponseSchema = maps:remove(<<"$schema">>, ResponseSchema),
                            #{
                                <<"description">> => <<"Success">>,
                                <<"content">> => #{
                                    <<"application/json">> => #{
                                        <<"schema">> => CleanResponseSchema
                                    }
                                }
                            }
                    end,
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

