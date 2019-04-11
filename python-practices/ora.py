from __future__ import print_function

import cx_Oracle

con = cx_Oracle.connect('APPFOLLOWUSR/APPFOLLOWUSR@gcomdv-scan.gartner.com')
print(con.version)

con.close()
