An implementation of part of JIRA's API and a simple client for listing and
searching issues.

If you have a sane lisp environment setup (only SBCL tested, at the moment)
with Quicklisp, running test-client.lisp should produce an executable:

```
Usage: jira-client [-ihv] [OPTIONS] ARGUMENTS...

A command line client for Jira issues
Main actions
  -lp, --list-projects        List available JIRA projects
  -is, --get-issues           list issues
  -i, --get-issue             show an issue
  -pi, --post-issue           post and issue
JIRA options
  --jira-account=URL-SUBDOMAIN The jira account to use.
                              Default: atomampd
Filtering Issues
  -s, --status=STR            Only show issues with a certain status
Other options
  -h, --help                  Show this help
  -v, --version               Show the program version
```
