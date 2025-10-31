-module(schema_draft04_converter).
-export([extract_request_response_schemas/1]).

%% Returns #{request => SchemaOrUndefined, responses => #{Code => SchemaOrUndefined}}
extract_request_response_schemas(Def) ->
    Req = case maps:get(<<"requestBody">>, Def, undefined) of
        undefined -> undefined;
        RB -> media_schema(RB)
    end,
    Resps = case maps:get(<<"responses">>, Def, undefined) of
        R when is_map(R) -> maps:from_list([resp_schema(K, V) || {K, V} <- maps:to_list(R)]);
        _ -> #{}
    end,
    #{request => Req, responses => Resps}.

media_schema(#{<<"content">> := Content}) when is_map(Content) ->
    case maps:get(<<"application/json">>, Content, undefined) of
        undefined -> undefined;
        #{<<"schema">> := S} -> to_draft04(S)
    end;
media_schema(_) -> undefined.

resp_schema(Code, #{<<"content">> := _}=Def) ->
    {Code, media_schema(Def)};
resp_schema(Code, _) -> {Code, undefined}.

to_draft04(Schema=#{}) ->
    %% Best-effort: assume incoming schema is compatible or close; tag $schema
    Schema#{<<"$schema">> => <<"http://json-schema.org/draft-04/schema#">>};
 to_draft04(Other) -> Other.
