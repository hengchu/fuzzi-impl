#!/usr/bin/python

import cgi
import cgitb; cgitb.enable()
import sys
import os
import subprocess
import json

(FUZZI_PATH, _) = os.path.split(sys.argv[0])
FUZZI_PATH = os.path.join(os.path.abspath(FUZZI_PATH), 'fuzzi')


def run_fuzzi(code, mode, **kwargs):
    if mode == 'typecheck':
        p = subprocess.Popen([FUZZI_PATH, '-I', 'fuzzi-lib/stdexts.fuzzi'],
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE,
                             stdin=subprocess.PIPE)
        (out, err) = p.communicate(input=code)

        if out:
            out_parsed = json.loads(out)
            out = json.dumps(out_parsed, indent=4, sort_keys=True)

        return (out, err)
    elif mode == 'pythonify':
        p = subprocess.Popen([FUZZI_PATH, '-I', 'fuzzi-lib/stdexts.fuzzi', '-t', kwargs['json']],
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE,
                             stdin=subprocess.PIPE)
        (out, err) = p.communicate(input=code)

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

    #fuzziform {
      display: flex;
      flex-direction: column;
    }

    #fuzziform div {
    }
    </style>
    </head>

    <body>

      <h3> Fuzzi Typechecker </h3>
      <p> Some example code can be found <a href="https://github.com/hengchu/fuzzi-impl/tree/master/testdata/new">here</a> </p>
      <p> The defined extensions can be found <a href="https://github.com/hengchu/fuzzi-impl/blob/master/fuzzi-lib/stdexts.fuzzi">here</a> </p>
      <p> You can define your own extensions, but they will only be typechecked using the core rules after expansion </p>
    """

    form = cgi.FieldStorage()
    code = form.getvalue("fuzzi", "")
    json = form.getvalue("json-path", "")
    mode = form.getvalue("run", "")

    if mode not in ['typecheck', 'pythonify']:
        mode = 'typecheck'

    if not code:
        print """
<textarea name="fuzzi" form="fuzziform" id="codearea">
/* Enter fuzzi code here... */
</textarea>

<p>(%s)</p>

<form id="fuzziform" method="post" action="index.cgi">
  <div>
    <input type="submit" name="run" value="typecheck"/>
  </div>
  <div>
    <input type="text" name="json-path" placeholder="enter path to data.json" />
    <input type="submit" name="run" value="pythonify"/>
  </div>
</form>
""" % (cgi.escape(FUZZI_PATH))

    else:
        out = None
        err = None
        if mode == 'typecheck':
            (out, err) = run_fuzzi(code, mode)
        elif mode == 'pythonify':
            (out, err) = run_fuzzi(code, mode, json=json)

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
