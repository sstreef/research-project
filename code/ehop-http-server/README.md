# HTTP server with effect handlers
The HTTP server is written as some sorts of a library that can be used by a developer to handle HTTP requests over TCP sockets. A practical demonstration of how this HTTP server can be used, can be found <a href="app/Main.hs">here</a>.

## Implementation details
This server was implemented using the <a href="https://github.com/polysemy-research/polysemy">Polysemy</a> library in <i>Haskell</i> and makes use of effects and effect handlers to maintain and develop this server.

## Using this HTTP server
_**Important notice:** this library was written for academic research purposes and is by no means developed to be used for any production builds. Use of this library is at your own risk._

The user of this HTTP server library can use an effect called `RequestHandling` which provides 5 effectful methods, namely:
<ul>
  <li><code>register :: MethodType -> String -> HTTPHandler -> Sem r ()</code></li>
  <li><code>resolveRequest :: HTTPRequest -> Sem r (Maybe HTTPResponse)</code></li>
  <li><code>resolveFileRequest :: HTTPRequest -> Sem r (Maybe HTTPResponse)</code></li>
  <li><code>SetStaticFilePath :: String -> Sem r ()</code></li>
  <li><code>GetStaticFilePath :: Sem r (Maybe String)</code></li>
</ul>

For users using this effect only `register` and `SetStaticFilePath` are considered to be useful. The `register` function allows the user to define a callback for a specific HTTP method and path combination to answer the request. The `setStaticFilePath` allows the user to set a resource path for `GET` requests which then allows requests to be resolved according to the files in this directory.
