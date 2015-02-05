# httpd0

**Description:**

A simple but effective HTTP/1.0 server with support for GET and HEAD
requests. *httpd0* is lightweight and reasonably fast. It can serve
static files out of the box and its modular design allows for easy
implementation of custom responders. In addition to the mandatory
headers, httpd0 also supports the *Last-Modified* and *If-Modified-Since*
headers. It's downwards compatible to HTTP/0.9 clients.

Things httpd0 does not support:

* PUSH requests
* Query parameters (implemented in the `query-parameters` branch if you
  really want them)
* Response compression
* SSL
* A lot of headers
* HTTP/1.1

Current stable version is
[1.1](https://github.com/eugeneia/httpd0/releases/tag/1.1).

**Documentation:**

* [Manual](http://mr.gy/software/httpd0/manual.html)
* [API documentation](http://mr.gy/software/httpd0/api.html)

**Dependencies:**

* [q-thread-pool](https://github.com/eugeneia/q-thread-pool)
* [net-telent-date](https://github.com/eugeneia/net-telent-date)
* [mpc](https://github.com/eugeneia/mpc)
* [file-types](https://github.com/eugeneia/file-types)
* [pretty-string](https://github.com/eugeneia/pretty-string)
* [percent-encoding](https://github.com/llibra/percent-encoding)
* [usocket](http://common-lisp.net/project/usocket/)
* [trivial-utf-8](http://common-lisp.net/project/trivial-utf-8/)
* [cl-fad](http://weitz.de/cl-fad/)
