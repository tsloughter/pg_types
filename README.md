pg_types
=====

Erlang library for encoding and decoding postgres data types.

## How it works

A type module implements the `pg_types` behaviour. `init/1` must return a 2-tuple with the first element being a list of `typsend` binary strings. `typsend` is what Postgres calls the conversion function and has defined for each OID. The second element is any sort of configuration the type needs included when encoding or decoding. For example the json implementation might want to support setting the json module to use when encoding and decoding.

`encode/2` and `decode/2` are passed the valuel to encode or decode and a `type_info` record. The record contains the configuration returned from `init/1` under `config`, so `#type_info{config=Config}`. The other fields are columns from Postgres' [pg_type](https://www.postgresql.org/docs/9.2/catalog-pg-type.html) table.

## Using

`pg_type:update/3` will lookup all type modules, based on the modules in `pg_types` and the `pg_types` application environment variable `modules`, which is how to add implementations not found in this application. The update function takes an atom namespace that usually corresponds to whatever pool the client is setting up types for, a list of `type_info` records populated from the Postgres table's rows and any option map to pass to each type module's `init/1`. It then sets a `persistent_term` with the `type_info` record updated to include the module that implement's the types `typsend` and the configuration returned from `init/1`.

To find the module for encoding/decoding an oid use `lookup_type_info/2`, passing the namespace (pool) and oid, it will return the `type_info` record with `module` and `config` set.

### Incomplete Types

* Range: need to implement inclusive/exclusive bounds. This will likely utilize a 3-tuple like `{1, 4, '(]'}`
* Timezones: I don't think the `timestamptz` is done
* Not a type but need to decide on the atoms for and define macros for nan, infinity, -infinity and make sure all types use them
* Timestamps: currently only supports integer timestamps. Not sure if there is really a reason to support the old float version?

### Missing Types

Type still to implement:

* composite types
* circle, line, line_segment, path, polygon
* tid
* tsvector
* record
* hstore
* bit_string
