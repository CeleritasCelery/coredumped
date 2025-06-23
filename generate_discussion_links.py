#!/usr/bin/env python3

import argparse
import urllib.parse

def generate_org_links(url: str, email: str = "troy@troyhinckley.com") -> str:
    """
    Generates an org-mode formatted string with discussion links for a given URL.
    """
    encoded_url_standard = urllib.parse.quote_plus(url)

    # Prepare URL for Reddit search (e.g., "url:domain.com/path/")
    # The part after "url:" should be the domain + path from the original URL, then URL encoded.
    parsed_url = urllib.parse.urlparse(url)
    
    # Construct the string to be encoded for the 'q' parameter for Reddit.
    # This is typically domain + path.
    # Example: if url is "https://example.com/foo/", then reddit_query_content is "example.com/foo/"
    reddit_query_content = parsed_url.netloc + parsed_url.path
    
    # Include query parameters from original URL if present, as they might be significant for identification.
    if parsed_url.query:
        reddit_query_content += "?" + parsed_url.query
    # Include fragment if present, though search engines might ignore it.
    if parsed_url.fragment:
        reddit_query_content += "#" + parsed_url.fragment

    encoded_reddit_query_content = urllib.parse.quote_plus(reddit_query_content)

    hn_link = f"https://hn.algolia.com/?dateRange=all&page=0&prefix=false&query={encoded_url_standard}&sort=byPopularity&type=story"
    lobsters_link = f"https://lobste.rs/search?q={encoded_url_standard}&what=stories&order=newest"
    # Reddit search query is "url:yourdomain.com/path/to/article"
    # The "url:" part is literal, the rest is the encoded domain+path
    reddit_link = f"https://www.reddit.com/search/?q=url%3A{encoded_reddit_query_content}&sort=top"

    return (
        f"Join the discussion on [[{hn_link}][HN]], "
        f"[[{lobsters_link}][Lobsters]], "
        f"[[{reddit_link}][Reddit]], "
        f"or send me an [[mailto:{email}][email]]."
    )

if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Generate org-mode discussion links for a blog post URL."
    )
    parser.add_argument("url", help="The URL of the blog post.")
    parser.add_argument(
        "--email",
        default="troy@troyhinckley.com",
        help="The email address to use for the mailto link. Default: troy@troyhinckley.com"
    )
    
    args = parser.parse_args()
    
    output_line = generate_org_links(args.url, args.email)
    print(output_line)
