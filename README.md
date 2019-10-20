#### pr-summary

Simple command line utility that pulls all open PRs for your organization on GitHub, and shows the output in a helpful form. The intent is something that can be run daily to view PR activity to help make informed decisions about when to do code review.

Written in Haskell for fun and learning.

Requires a config file `~/.pr-summary`, which should contain three lines:
1. github username
2. github personal access token
3. github org name
