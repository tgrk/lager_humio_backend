# lager_humio_backend
Lager backend for Humio log management system

## Requirements

Project depends on [jiffy][3] library for JSON parsing and uses default HTTP client (httpc).

## Usage

```
$ rebar3 update compile
```
or
```
$ rebar get-deps compile
```

## Setups
```
TODO
```

## Configuartion

| Option           | Required | Description                                    |
| ---------------- |:--------:| ---------------------------------------------- |
| token            | Yes      | Humio Ingestion API token (from [Settings][2]) |
| dataspce         | Yes      | Humio dataspace (from [Settings][2])           |
| level            | Yes      | Minimal log level to use (defaults to `debug`) |
| formatter        | No       | The module to use when formatting log messages (defaults to `lager_default_formatter') |
| formatter_config | No       | The format configuration string (defaults to `time [ severity ] message') |
| retry_interval   | No       | Intervarl for retry in case endpoint is not available (defaults to 60 seconds) |
| max_retries      | No       | Maximum number of retries (defaults to 10 retries) |
| httpc_opts       | No       | Set custom `httpc:http_options()` to change default HTTP client behaviour |

Example:
```erlang
{lager, [
  {handlers, [
    {lager_console_backend, debug},
    {lager_humio_backend,   [{token, "YOUR_INGESTION_API_TOKEN"},
                             {dataspace, "YOUR_DATASPACE"},
                             {level, debug},
                             {retry_interval, 60}, % optional
                             {max_retries,  10},   % optional
                             {httpc_opts,  []}     % optional
    ]}
  ]}
}

```

[1]: https://go.humio.com/docs/first-time-use/index.html
[2]: https://go.humio.com/docs/ingest-tokens/index.html
[3]: https://github.com/davisp/jiffy
