#!/bin/sh
ERLC_FLAGS="+debug_info -W2 -I ../include -o ../ebin -pa ../ebin"
MODULES="util.erl *.erl"
cp src/flashpolicy.app.src ebin/flashpolicy.app && cd src && erlc  $ERLC_FLAGS} -v $MODULES
if [ $? -eq 0 ]; then
  echo
  echo "Build succeeded. To continue..."
  echo
  echo "1. Edit flashpolicy.xml"
  echo "2. Copy your valid Certificate to ./ssl/host.cert"
  echo "3. Copy you secret Certificate key to ./ssl/host.key"
  echo "4. Start server: \`sudo ./policyserver start\`"
  echo "5. Test server : \`./policyserver test\`"
  echo "6. Stop server : \`./policyserver stop\`"
fi
cd ..
