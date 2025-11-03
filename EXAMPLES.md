# Examples and Templates

This document provides templates and examples for working with rebar3_openapi.

## Table of Contents

- [OpenAPI Specification Template](#openapi-specification-template)
- [Handler Module Template](#handler-module-template)
- [JSON Schema Templates](#json-schema-templates)
- [Common Patterns](#common-patterns)
- [Response Status Codes](#response-status-codes)

---

## OpenAPI Specification Template

### Minimal OpenAPI Spec

```yaml
openapi: 3.0.3
info:
  title: My API
  version: 1.0.0
  description: API description

paths:
  /api/v1/users:
    get:
      summary: Get users list
      operationId: getUserList
      responses:
        '200':
          description: Success
          content:
            application/json:
              schema:
                type: array
                items:
                  type: object

    post:
      summary: Create user
      operationId: createUser
      requestBody:
        required: true
        content:
          application/json:
            schema:
              type: object
              required: [name, email]
              properties:
                name:
                  type: string
                  minLength: 3
                email:
                  type: string
                  format: email
      responses:
        '201':
          description: Created
          content:
            application/json:
              schema:
                type: object
                properties:
                  id:
                    type: string
                  name:
                    type: string
                  email:
                    type: string
        '400':
          description: Invalid input
```

### Enhanced OpenAPI Spec

```yaml
openapi: 3.0.3
info:
  title: User Management API
  version: 1.0.0
  description: |
    Comprehensive user management API with authentication,
    CRUD operations, and advanced search capabilities.
  contact:
    name: API Team
    email: api@example.com

servers:
  - url: https://api.example.com
    description: Production
  - url: https://staging-api.example.com
    description: Staging
  - url: http://localhost:8080
    description: Local development

security:
  - bearerAuth: []

tags:
  - name: Users
    description: User management operations
  - name: Admin
    description: Administrative operations

paths:
  /api/v1/users/{userId}:
    get:
      summary: Get user by ID
      description: Retrieves detailed information for a specific user
      operationId: getUserById
      tags: [Users]
      parameters:
        - name: userId
          in: path
          required: true
          description: Unique user identifier
          schema:
            type: string
            pattern: '^[a-z0-9-]+$'
      responses:
        '200':
          description: User found
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/User'
        '404':
          description: User not found
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/Error'

components:
  securitySchemes:
    bearerAuth:
      type: http
      scheme: bearer
      bearerFormat: JWT

  schemas:
    User:
      type: object
      required: [id, name, email, createdAt]
      properties:
        id:
          type: string
          description: Unique user identifier
        name:
          type: string
          minLength: 3
          maxLength: 100
        email:
          type: string
          format: email
        createdAt:
          type: string
          format: date-time

    Error:
      type: object
      required: [error, message]
      properties:
        error:
          type: string
          description: Error code
        message:
          type: string
          description: Human-readable error message
        traceId:
          type: string
          description: Request trace ID for debugging
```

---

## Handler Module Template

### Minimal Handler

```erlang
-module(my_handler).
-export([routes/0, handle_request/3]).

routes() ->
    [
        #{
            path => "/api/v1/resource",
            allowed_methods => #{
                <<"get">> => #{operation_id => getResource}
            }
        }
    ].

handle_request(getResource, _Req, _Context) ->
    Code = 200,
    RespBody = #{data => []},
    {Code, RespBody};

handle_request(OperationId, _Req, Context) ->
    RespBody = #{message => <<"Not implemented">>},
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    {501, RespBody, Context, RespHeaders}.
```

### Full-Featured Handler

```erlang
-module(users_handler).
-export([routes/0, handle_request/3]).

routes() ->
    [
        #{
            path => "/api/v1/users",
            allowed_methods => #{
                <<"get">> => #{operation_id => getUserList}
            }
        },
        #{
            path => "/api/v1/users",
            allowed_methods => #{
                <<"post">> => #{
                    operation_id => createUser,
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        },
        #{
            path => "/api/v1/users/:userId",
            allowed_methods => #{
                <<"get">> => #{operation_id => getUserById}
            }
        },
        #{
            path => "/api/v1/users/:userId",
            allowed_methods => #{
                <<"put">> => #{
                    operation_id => updateUser,
                    content_types_accepted => [{<<"application">>, <<"json">>, '*'}]
                }
            }
        },
        #{
            path => "/api/v1/users/:userId",
            allowed_methods => #{
                <<"delete">> => #{operation_id => deleteUser}
            }
        }
    ].

%% GET /api/v1/users - List users
handle_request(getUserList, Req, _Context) ->
    %% Extract query parameters
    Limit = maps:get(<<"limit">>, Req, 20),
    Offset = maps:get(<<"offset">>, Req, 0),

    %% Call business logic
    {ok, Users} = users_service:list_users(Limit, Offset),

    {200, #{users => Users}};

%% POST /api/v1/users - Create user
handle_request(createUser, #{decoded_req_body := ReqBody} = _Req, _Context) ->
    Name = maps:get(<<"name">>, ReqBody),
    Email = maps:get(<<"email">>, ReqBody),

    case users_service:create_user(Name, Email) of
        {ok, User} ->
            {201, User};
        {error, validation_error} ->
            {400, #{error => <<"validation_error">>, message => <<"Invalid input">>}};
        {error, duplicate_email} ->
            {409, #{error => <<"duplicate">>, message => <<"Email already exists">>}}
    end;

%% GET /api/v1/users/:userId - Get user by ID
handle_request(getUserById, #{path_params := PathParams} = _Req, _Context) ->
    UserId = maps:get(<<"userId">>, PathParams),

    case users_service:get_user(UserId) of
        {ok, User} ->
            {200, User};
        {error, not_found} ->
            {404, #{error => <<"not_found">>, message => <<"User not found">>}}
    end;

%% PUT /api/v1/users/:userId - Update user
handle_request(updateUser, #{path_params := PathParams, decoded_req_body := ReqBody} = _Req, _Context) ->
    UserId = maps:get(<<"userId">>, PathParams),

    case users_service:update_user(UserId, ReqBody) of
        {ok, User} ->
            {200, User};
        {error, not_found} ->
            {404, #{error => <<"not_found">>, message => <<"User not found">>}};
        {error, validation_error} ->
            {400, #{error => <<"validation_error">>, message => <<"Invalid input">>}}
    end;

%% DELETE /api/v1/users/:userId - Delete user
handle_request(deleteUser, #{path_params := PathParams} = _Req, _Context) ->
    UserId = maps:get(<<"userId">>, PathParams),

    case users_service:delete_user(UserId) of
        ok ->
            {204, #{}};
        {error, not_found} ->
            {404, #{error => <<"not_found">>, message => <<"User not found">>}}
    end;

%% Catch-all for unimplemented operations
handle_request(OperationId, _Req, Context) ->
    RespBody = #{
        message => <<"Not implemented">>,
        operation => OperationId
    },
    RespHeaders = #{<<"content-type">> => <<"application/json">>},
    {501, RespBody, Context, RespHeaders}.
```

---

## JSON Schema Templates

### Operation Schema with Request Body

**File:** `apps/my_app/priv/json_schemas/createUser.json`

```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "operation_id": "createUser",
  "path": "/api/v1/users",
  "method": "POST",
  "summary": "Create a new user",
  "description": "Creates a user account with the provided information",
  "tags": ["Users"],
  "requestBody": {
    "required": true,
    "content": {
      "application/json": {
        "schema": {
          "type": "object",
          "required": ["name", "email"],
          "properties": {
            "name": {
              "type": "string",
              "description": "User's full name",
              "minLength": 3,
              "maxLength": 100,
              "example": "John Doe"
            },
            "email": {
              "type": "string",
              "description": "User's email address",
              "format": "email",
              "example": "john.doe@example.com"
            },
            "age": {
              "type": "integer",
              "description": "User's age",
              "minimum": 18,
              "maximum": 120,
              "example": 30
            }
          }
        }
      }
    }
  },
  "responses": {
    "201": {
      "description": "User created successfully",
      "content": {
        "application/json": {
          "schema": {
            "type": "object",
            "properties": {
              "id": {
                "type": "string",
                "example": "user-12345"
              },
              "name": {
                "type": "string",
                "example": "John Doe"
              },
              "email": {
                "type": "string",
                "example": "john.doe@example.com"
              },
              "createdAt": {
                "type": "string",
                "format": "date-time",
                "example": "2025-11-03T10:30:00Z"
              }
            }
          }
        }
      }
    },
    "400": {
      "description": "Invalid input"
    },
    "409": {
      "description": "User with this email already exists"
    }
  }
}
```

### Operation Schema with Path Parameters

**File:** `apps/my_app/priv/json_schemas/getUserById.json`

```json
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "operation_id": "getUserById",
  "path": "/api/v1/users/{userId}",
  "method": "GET",
  "summary": "Get user by ID",
  "description": "Retrieves detailed information for a specific user",
  "tags": ["Users"],
  "parameters": [
    {
      "name": "userId",
      "in": "path",
      "required": true,
      "description": "Unique user identifier",
      "schema": {
        "type": "string",
        "pattern": "^user-[a-z0-9]+$",
        "example": "user-12345"
      }
    }
  ],
  "responses": {
    "200": {
      "description": "User found",
      "content": {
        "application/json": {
          "schema": {
            "type": "object",
            "properties": {
              "id": {"type": "string"},
              "name": {"type": "string"},
              "email": {"type": "string"}
            }
          }
        }
      }
    },
    "404": {
      "description": "User not found"
    }
  }
}
```

### Global Metadata File

**File:** `apps/my_app/priv/json_schemas/_openapi_metadata.json`

```json
{
  "openapi": "3.0.3",
  "info": {
    "title": "My API",
    "version": "1.0.0",
    "description": "Comprehensive API for user management",
    "contact": {
      "name": "API Team",
      "email": "api@example.com"
    },
    "license": {
      "name": "Apache 2.0",
      "url": "https://www.apache.org/licenses/LICENSE-2.0"
    }
  },
  "servers": [
    {
      "url": "https://api.example.com",
      "description": "Production server"
    },
    {
      "url": "http://localhost:8080",
      "description": "Local development"
    }
  ],
  "security": [
    {
      "bearerAuth": []
    }
  ],
  "tags": [
    {
      "name": "Users",
      "description": "User management operations"
    },
    {
      "name": "Admin",
      "description": "Administrative operations"
    }
  ],
  "components": {
    "securitySchemes": {
      "bearerAuth": {
        "type": "http",
        "scheme": "bearer",
        "bearerFormat": "JWT",
        "description": "JWT token for authentication"
      }
    },
    "schemas": {
      "Error": {
        "type": "object",
        "required": ["error", "message"],
        "properties": {
          "error": {
            "type": "string",
            "description": "Error code",
            "example": "validation_error"
          },
          "message": {
            "type": "string",
            "description": "Human-readable error message",
            "example": "Invalid input provided"
          },
          "traceId": {
            "type": "string",
            "description": "Trace ID for debugging",
            "example": "trace-abc-123"
          }
        }
      }
    }
  }
}
```

---

## Common Patterns

### Query Parameters

```yaml
parameters:
  - name: limit
    in: query
    description: Maximum number of results to return
    schema:
      type: integer
      minimum: 1
      maximum: 100
      default: 20
  - name: offset
    in: query
    description: Number of results to skip
    schema:
      type: integer
      minimum: 0
      default: 0
  - name: sort
    in: query
    description: Sort field and order
    schema:
      type: string
      enum: [name_asc, name_desc, created_asc, created_desc]
      default: created_desc
```

### Array Response

```yaml
responses:
  '200':
    description: List of users
    content:
      application/json:
        schema:
          type: object
          properties:
            users:
              type: array
              items:
                $ref: '#/components/schemas/User'
            total:
              type: integer
            limit:
              type: integer
            offset:
              type: integer
```

### Reusable Schemas

```yaml
components:
  schemas:
    User:
      type: object
      required: [id, name, email]
      properties:
        id:
          type: string
        name:
          type: string
        email:
          type: string
          format: email

    Error:
      type: object
      required: [error, message]
      properties:
        error:
          type: string
        message:
          type: string

    PaginatedResponse:
      type: object
      required: [data, total, limit, offset]
      properties:
        data:
          type: array
          items: {}
        total:
          type: integer
        limit:
          type: integer
        offset:
          type: integer
```

### Validation Constraints

```json
{
  "type": "string",
  "minLength": 3,
  "maxLength": 100,
  "pattern": "^[A-Za-z0-9-]+$",
  "format": "email"
}

{
  "type": "integer",
  "minimum": 0,
  "maximum": 100,
  "multipleOf": 5
}

{
  "type": "array",
  "minItems": 1,
  "maxItems": 10,
  "uniqueItems": true
}

{
  "type": "object",
  "required": ["field1", "field2"],
  "additionalProperties": false
}
```

---

## Response Status Codes

### Standard HTTP Status Codes

| Code | Name                | Use Case                                   | Handler Return          |
|------|---------------------|--------------------------------------------|-------------------------|
| 200  | OK                  | Successful GET/PUT                         | `{200, Data}`           |
| 201  | Created             | Successful POST                            | `{201, NewResource}`    |
| 204  | No Content          | Successful DELETE                          | `{204, #{}}`            |
| 400  | Bad Request         | Validation error, malformed input          | `{400, ErrorMap}`       |
| 401  | Unauthorized        | Missing or invalid authentication          | `{401, ErrorMap}`       |
| 403  | Forbidden           | Authenticated but insufficient permissions | `{403, ErrorMap}`       |
| 404  | Not Found           | Resource doesn't exist                     | `{404, ErrorMap}`       |
| 409  | Conflict            | Duplicate resource, constraint violation   | `{409, ErrorMap}`       |
| 422  | Unprocessable       | Valid format but semantic error            | `{422, ErrorMap}`       |
| 429  | Too Many Requests   | Rate limit exceeded                        | `{429, ErrorMap}`       |
| 500  | Internal Error      | Unexpected server error                    | `{500, ErrorMap}`       |
| 501  | Not Implemented     | Operation not yet implemented              | `{501, ErrorMap}`       |
| 503  | Service Unavailable | System maintenance or overload             | `{503, ErrorMap}`       |

### Error Response Format

Standard error response structure:

```json
{
  "error": "error_code",
  "message": "Human-readable error message",
  "traceId": "request-trace-id",
  "details": {
    "field": "Additional context"
  }
}
```

Example in handler:

```erlang
handle_request(createUser, #{decoded_req_body := ReqBody} = _Req, _Context) ->
    case validate_user(ReqBody) of
        ok ->
            {ok, User} = users_service:create(ReqBody),
            {201, User};
        {error, {invalid_field, Field}} ->
            ErrorBody = #{
                error => <<"validation_error">>,
                message => <<"Invalid input">>,
                details => #{field => Field}
            },
            {400, ErrorBody}
    end.
```

---

## Complete Example

See `examples/` directory for working examples:
- `examples/specs/sample.yaml` - Full OpenAPI specification
- `examples/handlers/min_handler.erl` - Minimal handler
- `examples/schemas/` - JSON schema files with metadata

