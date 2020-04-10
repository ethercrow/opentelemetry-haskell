
This is an example project showcasing the tracing of some popular libraries, currently wai, warp and http-client.

You can build and run it locally by executing the following:

```
$ stack --stack-yaml stack-8.8.2.yaml run -- megaexample +RTS -T -l -olmegaexample.eventlog
```

Now we can simulate a typical enterprise industrial workload of getting some data over HTTP and returning it over HTTP:

```
$ curl http://127.0.0.1:6502/http/example.com
```

Or even go deeper:

```
$ curl http://127.0.0.1:6502/http/localhost:6502/http/example.com
```

Or ask the service to perform a GC:

```
$ curl http://127.0.0.1:6502/gc
```

After stopping the megaexample service we can upload `megaexample.eventlog` to LightStep:

```
eventlog-to-lightstep megaexample.eventlog
```

TODO: describe how to load the eventlog in Chrome or Tracy
