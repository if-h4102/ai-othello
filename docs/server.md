# How to use Prolog server

## Run it

At the moment, a basic server is available under `src/server.pl`.
You can run it using:

    swipl src/server.pl
    server(8000).
    
## What it can do
    
You can see the result of the previous commands by opening http://127.0.0.1:8000 in your favorite browser.

At the moment, a test API endpoint is also available:
given a number, it can send you the resulting square number, in JSON format.

To test it, just type the following URL in your favorite browser: http://127.0.0.1:8000/square?number=8.
It will result in:

    {
      number:8,
      square:64
    }
    
Of course, you can do it with other numbers than 8.

## How does it works

The basic structure for creating an enpoint is the following one:

```
http_handler('your/path', your_handling_predicat, []).
your_handling_predicat(Request) :-
  http_parameters(
    Request, [
      your_param1(Your_param1, [<condition>]),
      ...
      your_paramN(Your_paramN, [<condition>])
    ]),
    whatever_you_need_to_do,
    reply_json(json([
      item_name=value,
      object_name=json([
        item_name=value
      ])
    ])).
```
