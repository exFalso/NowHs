NowHs
=====

Call server-side Haskell from client browsers over Websockets.

Or: *Let your browsers talk to a backend, the awesomely safe way.*


Calling a server side function
------------------------------

On the server:

```haskell
sum_rpc :: Int -> Int -> NowHs () Int
sum_rpc a b = return (a + b)
```

In the browser:

```javascript
var server = new NowHs("localhost", 8888, function () {

  server.sum_rpc(1, 2, function(res) { console.log(res); });

});
```

The remote function call is "as typesafe as Javascript allows":
Calling `server.sum_rpc("1", 2, ...)` will result in an error message like

```
Mismatching types: sum_rpc: Argument 0 was 'String', expected 'Int'
```

Note that this type check is done *in Javascript*, based on
[schemas](https://github.com/nh2/jsschema) auto-generated from the
function's type signature, and can also be used manually,
e.g. for user input validation.

When you've worked on browser-server-interfaces before, e.g. in Python or Node.js,
this might be *the thing* for you.


Working with data types
-----------------------

NowHs can automatically convert your custom data types to JSON.

```haskell
data User = User { name :: String, age :: Int }

$(deriveJSON id ''User)
$(deriveSchema ''User)

authenticated_rpc :: User -> NowHs () Bool
authenticated_rpc user = return $ age user >= 18

firstUser_rpc :: NowHs () User
firstUser_rpc = return $ User { name = "John", age = 17 }
```
