#!/bin/sh
ERLC_FLAGS="+debug_info -W2 -I ../include -o ../ebin -pa ../ebin"
MODULES="util.erl *.erl"
cp src/flashpolicy.app.src ebin/flashpolicy.app && cd src && erlc  $ERLC_FLAGS} -v $MODULES
if [ $? -eq 0 ]; then
  echo
  echo "Build succeeded. To continue..."
  echo
  echo "1. Edit flashpolicy.xml"
  echo "2. Start server: \`sudo ./policyserver start\`"
  echo "3. Test server : \`./policyserver test\`"
  echo "4. Stop server : \`./policyserver stop\`"
fi
cd ..
