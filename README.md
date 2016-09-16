# httpd0

**Description:**

A simple but effective HTTP/1.0 server for Clozure Common Lisp with support for
GET and HEAD requests. *httpd0* is lightweight and reasonably fast. It can
serve static files out of the box and its modular design allows for easy
implementation of custom responders. In addition to the mandatory headers,
httpd0 also supports the *Last-Modified* and *If-Modified-Since* headers. It’s
downwards compatible to HTTP/0.9 clients.

Things httpd0 does not support:

* PUSH requests
* Query parameters (implemented in the `query-parameters` branch if you
  really want them)
* Response compression
* SSL
* A lot of headers
* HTTP/1.1

Current stable version is
[1.2](https://github.com/eugeneia/httpd0/releases/tag/v1.2).

**Documentation:**

* [Manual](http://mr.gy/software/httpd0/manual.html)
* [API documentation](http://mr.gy/software/httpd0/api.html)

**Dependencies:**

* [q-thread-pool](https://github.com/eugeneia/q-thread-pool)
* [cl-date-time-parser](https://github.com/tkych/cl-date-time-parser)
* [maxpc](https://github.com/eugeneia/maxpc)
* [file-types](https://github.com/eugeneia/file-types)
* [percent-encoding](https://github.com/llibra/percent-encoding)
* [trivial-utf-8](http://common-lisp.net/project/trivial-utf-8/)
* [cl-fad](http://weitz.de/cl-fad/)
* [uiop](http://www.cliki.net/UIOP)
