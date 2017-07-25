# lager_humio_backend
Lager backend for Humio log management system

## Requirements
```
TODO
```

## Setups
```
TODO
```

## Configuartion

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
