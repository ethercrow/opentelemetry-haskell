
This is an example project showcasing the tracing of some popular libraries, currently wai, warp and http-client.

You can build and run it locally by executing the following:

```
$ stack --stack-yaml stack-8.8.2.yaml run -- megaexample --file megaexample.trace.json +RTS -T
```

Now we can simulate a typical enterprise industrial workload of getting some data over HTTP and returning it over HTTP:

```
$ curl http://127.0.0.1:6502/http/example.com
```

Or even go deeper:

```
$ curl http://127.0.0.1:6502/http/localhost:6502/http/example.com
```

After stopping the megaexample service we can load `megaexample.trace.json` trace file in Chrome or Tracy and see how this service delegated the request first to itself and then to http://example.com:

![megaexample_trace](https://i.imgur.com/mR6Ihn9.png)
