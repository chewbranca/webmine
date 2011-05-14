# webmine

## urls

Get a seq of urls, or remove all urls, from an arbitrary string containing urls.

    (url-seq raw-text)

## feeds (rss/atom)

Get the current entries for a feed.

    (entries feed-url)

Identify the canonical rss/atom feeds from a given seq of urls. 

    (canonical-feeds feed-urls)

## text and dom processing

	(readability-div d)

## Authors

- Copyright (c) Hang City Bang released under the MIT License (http://www.opensource.org/licenses/mit-license.php).

## Sponsors

YourKit is kindly supporting open source projects with its full-featured Java Profiler.
YourKit, LLC is the creator of innovative and intelligent tools for profiling
Java and .NET applications. Take a look at YourKit's leading software products:
[YourKit Java Profiler](http://www.yourkit.com/java/profiler/index.jsp) and
[YourKit .NET Profiler](http://www.yourkit.com/.net/profiler/index.jsp).
