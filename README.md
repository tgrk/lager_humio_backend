[![CircleCI](https://circleci.com/gh/tgrk/lager_humio_backend/tree/master.svg?style=svg)](https://circleci.com/gh/tgrk/lager_humio_backend/tree/master)
[![Hex pm](http://img.shields.io/hexpm/v/lager_humio_backend.svg?style=flat)](https://hex.pm/packages/lager_humio_backend)
[![codecov.io](https://codecov.io/github/tgrk/lager_humio_backend/coverage.svg?branch=master)](https://codecov.io/github/tgrk/lager_humio_backend?branch=master)

# lager_humio_backend
Erlang Lager backend for [Humio][1] log management system

## Requirements

* Erlang 18 or newer
* [Humio][1] account and API token

## Dependencies

Project depends on [jiffy][3] library for JSON parsing and uses default HTTP client (httpc).

## Usage

```
$ rebar3 update compile
```
or

```
$ rebar get-deps && rebar compile
```

## Setup
First you have to sign up with [Humio][1] service to get all required information for [setting][2] up logging backend.

## Configuaration

| Option             | Required | Description                                    |
| ------------------ |:--------:| ---------------------------------------------- |
| `token`            | Yes      | Humio Ingestion API token (from [Settings][2]) |
| `dataspce`         | Yes      | Humio dataspace (from [Settings][2])           |
| `level`            | Yes      | Minimal log level to use (defaults to `debug`) |
| `formatter`        | No       | The module to use when formatting log messages (defaults to `lager_default_formatter') |
| `formatter_config` | No       | The format configuration string (defaults to `time [ severity ] message`) |
| `metadata_filter`  | No       | A list of excluded metadata keys |
| `retry_interval`   | No       | Intervarl for retry in case endpoint is not available (defaults to 60 seconds) |
| `max_retries`      | No       | Maximum number of retries (defaults to 10 retries) |
| `httpc_opts`       | No       | Set custom `httpc:http_options()` to change default HTTP client behaviour |

Sample configuration:
```erlang
{lager, [
  {handlers, [
    {lager_console_backend, debug},
    {lager_humio_backend,   [{token, "YOUR_INGESTION_API_TOKEN"},
                             {dataspace, "YOUR_DATASPACE"},
                             {level, info}
    ]}
  ]}
}

```

[1]: https://go.humio.com/docs/first-time-use/index.html
[2]: https://go.humio.com/docs/ingest-tokens/index.html
[3]: https://github.com/davisp/jiffy
