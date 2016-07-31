[![Build Status](https://travis-ci.org/Minoru/hakyll-convert.svg?branch=master)](https://travis-ci.org/Minoru/hakyll-convert)

Hakyll-convert provides a library and utility that allows you to import
blogs from a variety of engines (currently, Blogger and Wordpress) to
the [hakyll][hakyll] static site generator.

We aim to

* avoid converting your content between formats (it usually comes in
  HTML and should stay as such without passing through filters)
* allow for the possibility of [Cool URIs][cool-uris] by keeping
  relative page names the same as on your old blog (this only works
  if you use your own domain for your hosted site). If you don't agree with us
  on that point, use `--output-format` flag to specify your own output filename
  format.

[hakyll]:    http://jaspervdj.be/hakyll/
[cool-uris]: http://www.w3.org/Provider/Style/URI.html
