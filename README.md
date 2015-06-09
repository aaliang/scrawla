About
-----
Extensible functional webcrawler.

You can determine which anchor links to follow, and normalization strategies for urls.
Base implementation will follow same domain links. Normalization is to consistently
predict variant urls that may appear, although this is optimistic, and may very well
request more urls than needed (in the worst case)

Http requests are dispatched asynchronously via a request agent actor, with an
overridable concurrency limit set to 6 (yet to be configured out)

Base crawlers should implement the BaseCrawler trait, and there are some abstracts that
need to be fulfilled. DefaultCrawler is the full implementation, and is invoked during
runs of the main program.

Extended crawlers should be able to easily override or implement the trait/Default class
to follow reference criteria as they see fit, and to apply any normalization strategies to
pass along links to the request dispatcher.

Build/Run
---------
> $ sbt "run google.com"

Requirements
------------
 Java. Works on Java 8, should work on 7+
 sbt

Known Issues
------------
- There __may__ be synchronization issues, determining if they actually exist for sure
is a WIP, but they are non-fatal if they do exist. More extensive proofing may be needed
- Currently defaults the initial request as http. As of now, this is set up to follow redirects
this is probably not ideal in some situations