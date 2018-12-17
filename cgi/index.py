#!/usr/bin/python

import cgi
import cgitb; cgitb.enable()
import sys
import os
import subprocess
import json

(FUZZI_PATH, _) = os.path.split(sys.argv[0])
FUZZI_PATH = os.path.join(os.path.abspath(FUZZI_PATH), 'fuzzi')


def run_fuzzi(code):
    p = subprocess.Popen([FUZZI_PATH, '-I', 'fuzzi-lib/stdexts.fuzzi'],
                                 stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE,
                                 stdin=subprocess.PIPE)
    (out, err) = p.communicate(input=code)

    if out:
        out_parsed = json.loads(out)
        out = json.dumps(out_parsed, indent=4, sort_keys=True)

    return (out, err)

def main():
    print "Content-type: text/html"
    print

    print """
    <html>

    <head>
    <title>Fuzzi Typechecker</title>
    <style>
    #codearea {
      min-width: 500px;
      min-height: 500px;
      font-family: monospace;
    }
    </style>
    </head>

    <body>

      <h3> Fuzzi Typechecker </h3>
    """

    form = cgi.FieldStorage()
    code = form.getvalue("fuzzi", "")

    if not code:
        print """
<textarea name="fuzzi" form="fuzziform" id="codearea">
/* Enter fuzzi code here... */
</textarea>

<p>(%s)</p>

<form id="fuzziform" method="post" action="index.cgi">
  <input type="submit"/>
</form>
""" % (cgi.escape(FUZZI_PATH))

    else:
        (out, err) = run_fuzzi(code)

        print """
<p>typechecking result:</p>
<pre>
%s
</pre>
<br>
<p>typechecking error:</p>
<pre>
%s
</pre>
""" % (cgi.escape(out), cgi.escape(err))

    print """
</body>
</html>
"""

if __name__ == '__main__':
    main()
