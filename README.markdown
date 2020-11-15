Hakyll-convert provides a library and a program for importing blogs from
a variety of engines (currently, Blogger and WordPress) to the [Hakyll][hakyll]
static site generator.

We aim to:

* avoid converting your content between formats (it usually comes in
  HTML and should stay as such without passing through filters)
* allow for the possibility of [Cool URIs][cool-uris] by keeping
  relative page names the same as on your old blog (this only works
  if you use your own domain for your hosted site). If you don't agree with us
  on that point, use `--output-format` flag to specify your own output filename
  format.

Usage
-----

1. You should make a dump (backup) of all the posts in your existing blog. For
   Blogger, see [this post in the Help
   Center](https://support.google.com/blogger/answer/41387). For WordPress,
   [see this support page](https://wordpress.com/support/export/).

2. Install `hakyll-convert` using `cabal-install`:

   ```console
   $ cabal install hakyll-convert
   ```
3. Assuming you have your backup in a file named backup.atom, run one of the following:

   - if it's a Blogger backup:

     ```console
     $ hakyll-convert --format=blogger backup.atom posts
     ```

   - if it's a WordPress backup:

     ```console
     $ hakyll-convert --format=wordpress backup.atom posts
     ```
4. You now have a directory named "posts" with HTML or Markdown files containing your posts.

For more control over the conversion, see `--output-format` and
`--extract-comments` options. For now, `--help` is the best documentation we
have (second only to the code. Sorry once again).

[hakyll]:    http://jaspervdj.be/hakyll/
[cool-uris]: http://www.w3.org/Provider/Style/URI.html
