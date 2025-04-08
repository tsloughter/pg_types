pg_types
=====

Erlang library for encoding and decoding postgres data types. Used by [pgo](https://github.com/erleans/pgo).

## How it works

A type module implements the `pg_types` behaviour. `init/1` must return a 2-tuple with the first element being a list of `typsend` binary strings. `typsend` is what Postgres calls the conversion function and has defined for each OID. The second element is any sort of configuration the type needs included when encoding or decoding. For example the json implementation might want to support setting the json module to use when encoding and decoding.

`encode/2` and `decode/2` are passed the valuel to encode or decode and a `type_info` record. The record contains the configuration returned from `init/1` under `config`, so `#type_info{config=Config}`. The other fields are columns from Postgres' [pg_type](https://www.postgresql.org/docs/9.2/catalog-pg-type.html) table.

## Using

`pg_types:update/3` will lookup all type modules, based on the modules in `pg_types` and the `pg_types` application environment variable `modules`, which is how to add implementations not found in this application. The update function takes an atom namespace that usually corresponds to whatever pool the client is setting up types for, a list of `type_info` records populated from the Postgres table's rows and any option map to pass to each type module's `init/1`. It then sets a `persistent_term` with the `type_info` record updated to include the module that implement's the types `typsend` and the configuration returned from `init/1`.

`pg_types:update/4` with a map as the last argument to update a map with `oid() => type_info()}` instead of having them inserted into a `persistent_term`.

To find the module for encoding/decoding an oid use `lookup_type_info/2`, passing the namespace (pool) and oid, it will return the `type_info` record with `module` and `config` set.

## Configuring

To configure if json is to be encoded and decoded by a library, include the json library, like `jsone`, in theprojects `deps` list and set `json_config` in `pg_types` application environment to a 3-tuple `{module(), encode_options(), decode_options()}`. For example to use `jsone` and decode keys as atoms:

### Json

`{pg_types, [{json_config, {jsone, [], [{keys, atom}]}}]}`

### Enums

To configure if enums should be converted to atoms, set `enum_config` in `pg_types` application environment to the atom `atoms`, or the atom `existing_atoms`. Using `existing_atoms` will result in `binary_to_existing_atom(Value, utf8)` being used to decode values.

`{pg_types, [{enum_config, atoms}]}`

### Datetimes

#### Timestamps
Timestamps can be returned as Erlang system time in seconds (as an integer or float):

`{pg_types, [{timestamp_config, float_system_time_seconds | integer_system_time_seconds}]}`

Future plans include allowing this to be set per-query instead of globally.

#### Timestamps with time zone
Timestamps with time zone can be returned as Erlang system time in rfc3339 binary:

`{pg_types, [{timestampz_config, rfc3339_bin}]}`

```
CREATE TABLE public.app_version (
    id bigserial NOT NULL,
    ...
    created_at timestamptz DEFAULT CURRENT_TIMESTAMP NOT NULL, -- 2025-02-21 08:33:16.268288+08:00
    CONSTRAINT app_version_pkey PRIMARY KEY (id)
);
```
```
(imboy@imboy.local)1> pgo:query("select created_at FROM app_version limit 1;").
Without configuration timestampz_config:
#{command => select,
  rows =>
      [#{<<"created_at">> =>
             {{2025,2,25},{23,45,34.492948}}],
  num_rows => 1}

With {pg_types, [{timestampz_config, rfc3339_bin}]} the result is:

#{command => select,
  rows =>
      [#{<<"created_at">> =>
             <<"2025-02-26 08:45:34.493366+08:00">>}],
  num_rows => 1}

```

Future plans include allowing this to be set per-query instead of globally.

### Incomplete Types

* Not a type but need to decide on the atoms for and define macros for nan, infinity, -infinity and make sure all types use them

## Acknowledgements

Most types are based on code from [epgsql](https://github.com/epgsql/epgsql), [semiocast/pgsql](https://github.com/semiocast/pgsql) and [postgrex](https://github.com/elixir-ecto/postgrex)
