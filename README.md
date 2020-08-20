## Erlang OTP application for handling MaxMind(tm) Geoip2 (.mmdb) files.

This is a forked branch of [altenmueller/geodata2](https://github.com/altenmueller/geodata2), which uses a shared `ETS` table to hold the database state rather than a cluster of workers.  
This was done to improve performance.

#### Features:
* Supports all types (hopefully) of bases in .mmdb format, both IPv4 and IPv6 ones.
* Safe from binary reference leakage, binary parts are getting copied.
* R18 compatible.


The app accepts IPs in `{B3:8, B2:8, B1:8, B0:8}`, `{W7:16, W6:16, W5:16, W4:16, W3:16, W2:16, W1:16, W0:16}`, string, binary string, and big-endian dword formats.


#### Example:

Download and unzip GeoLite2-City.mmdb from [MaxMind](http://dev.maxmind.com/geoip/geoip2/geolite2/) site and put the databases in the `priv/` dir.


```
1> geodata2:start().
ok
```

```
3> geodata2:lookup({94, 75, 242, 11}).
{ok,[{<<"registered_country">>,
      [{<<"names">>,
        [{<<"zh-CN">>,<<232,141,183,229,133,176>>},
         {<<"ru">>,
          <<208,157,208,184,208,180,208,181,209,128,208,187,208,
            176,208,189,208,...>>},
         {<<"pt-BR">>,<<"Holanda">>},
    ...
       {<<"geoname_id">>,6255148},
       {<<"code">>,<<"EU">>}]}]}
```

```
8> geodata2:lookup({127, 0, 0, 1}).
not_found
```

```
9> geodata2:lookup({127, 0, 0, 1, 1}).
{error,format}
```


*A MaxMind repo with a number of test bases: [MaxMind-DB](https://github.com/maxmind/MaxMind-DB/)*

*This application uses some bits and pieces from [egeoip](http://github.com/mochi/egeoip)*
