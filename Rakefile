# Common build system
require 'rubygems'
require 'rake'


class String
    def red; colorize(self, "\e[1m\e[31m"); end
    def green; colorize(self, "\e[1m\e[32m"); end
    def dark_green; colorize(self, "\e[32m"); end
    def yellow; colorize(self, "\e[1m\e[33m"); end
    def blue; colorize(self, "\e[1m\e[34m"); end
    def dark_blue; colorize(self, "\e[34m"); end
    def pur; colorize(self, "\e[1m\e[35m"); end
    def colorize(text, color_code)  "#{color_code}#{text}\e[0m" end
end

COOKIE = `head /dev/random | shasum | cut -d " " -f1 | tr -d '\n'`
NODE_NAME = "flashpolicy"

RUN_INCLUDE_PATHS = "-pa ./ebin -pa ./ebin/eunit -pa ./include -pa ./src"
ERLC_TEST_FLAGS = "#{RUN_INCLUDE_PATHS} -pa ../ebin/eunit -I .. -I ../test -I ../include/eunit -DTEST"
ERLC_FLAGS = "+debug_info -W2 -I ../include -o ../ebin -pa ../ebin"
MODULES = "util.erl *.erl"

# force epmd to bind to loopback interface only when started by erl command
ENV['ERL_EPMD_ADDRESS']="127.0.0.1"

desc "Compiles all files and writes the binaries to ./ebin"
task :build do
  cd "src"
  sh "erlc  #{ERLC_FLAGS} -v #{MODULES} && cp flashpolicy.app.src ../ebin/flashpolicy.app"
  cd ".."
end

desc "Compiles all files including test definition and writes the binaries to ./ebin"
task :build_test do
  cd "src"
  sh "erlc  #{ERLC_FLAGS} #{ERLC_TEST_FLAGS} -v #{MODULES}"
  cd ".."
end

desc "starts an erlang console with correct load path"
task :console do
  node_name = "console_#{Time.now.to_i}"
  sh "erl #{RUN_INCLUDE_PATHS} -sname #{node_name} -setcookie #{COOKIE}"
end

desc "removes the build binaries"
task :clean do
  cd "ebin"
  sh "rm -f *.beam"
  cd ".."
end

desc "shows the last log file"
task :log do
  logfile = `ls -1 -t log/*.log | head -n 1`
  if logfile != ""
    puts "showing logfile: #{logfile}"
    sh "tail -f -n 1000 #{logfile} "
  end
end

desc "starts the server as daemon"
task :start do
  sh "erl +K true #{RUN_INCLUDE_PATHS} -noshell -noinput -detached -sname #{NODE_NAME} -setcookie #{COOKIE} -run flashpolicy_app start"
end

desc "starts the server and opens the erlang console"
task :debug => [:build] do
  sh "erl +K true #{RUN_INCLUDE_PATHS} -sname #{NODE_NAME} -setcookie #{COOKIE} -run flashpolicy_app start"
end

task :monitor do
  host_name = `hostname -s`.strip
  monitor_name = "monitor_#{Time.now.to_i}"
  running_servers = `ps ax | grep beam | grep sname | grep cookie | grep -v grep | grep -v monitor`.strip
  servers = []
  running_servers.each_line do |server|
    if server.strip != ""
      /-sname\s+([^\s]+)\s+.*-setcookie\s+([^\s]+)\s+/ =~ server
      node_name = Regexp.last_match(1)
      cookie = Regexp.last_match(2)
      servers << { :node => node_name, :cookie => cookie}
    end
  end
  if servers.size == 0
    puts "no running servers found!"
  elsif servers.size == 1
    node_name = servers[0][:node]
    cookie = servers[0][:cookie]
    puts "connecting to node '#{node_name}' on host '#{host_name}' with cookie '#{cookie}'"
    sh "erl #{RUN_INCLUDE_PATHS} -setcookie #{cookie} -sname #{monitor_name} -remsh #{node_name}@#{host_name}"
  else
    puts "choose server ..."
    servers.each_with_index do |server, i|
      node_name = server[:node]
      cookie = server[:cookie]
      puts "[#{i+1}] '#{node_name}' on host '#{host_name}' with cookie '#{cookie}'"
    end
    i = STDIN.gets.to_i
    if i > 0 && i <= servers.size
      node_name = servers[i-1][:node]
      cookie = servers[i-1][:cookie]
      puts "connecting to node '#{node_name}' on host '#{host_name}' with cookie '#{cookie}'"
      sh "erl #{RUN_INCLUDE_PATHS} -setcookie #{cookie} -sname #{monitor_name} -remsh #{node_name}@#{host_name}"
    end
  end
end

task :test => [:build_test] do
  mods = Dir["test/*_test.erl"].map { |x| x.match(/test\/(.*)_test.erl/)[1] }

  has_test_failures = false
  total_tests = total_failed = total_passed = total_skipped = 0

  test_results = mods.map do |m|
    puts "Testing #{m} ..."
    test_command = "erl +K true -pz ./test -pa ./ebin/eunit #{RUN_INCLUDE_PATHS} -sname master_test #{env_variables} -noshell -run util test #{m} -run erlang halt"
    result = `#{test_command}`
    has_test_failures |= $? != 0
    last_line = result.split("\n").last.strip
    if match = (last_line.match(/All (\d*) tests passed\./))
      total_tests += match[1].to_i
      total_passed += match[1].to_i
    elsif last_line == 'Test passed'
      total_tests += 1
      total_passed += 1
    elsif match = (last_line.match(/Failed: (\d*)\.  Skipped: (\d*)\.  Passed: (\d*)\./))
      total_tests   += (match[1].to_i + match[2].to_i + match[3].to_i)
      total_failed  += match[1].to_i
      total_skipped += match[2].to_i
      total_passed  += match[3].to_i
    end
    puts result
  end

  puts "Total: #{total_tests} | Failed: #{total_failed} | Passed: #{total_passed} | Skipped: #{total_skipped}"
  fail(Exception.new("#{total_failed} test failed")) if has_test_failures
end

desc "Generates edoc documentation and writes it to the doc folder"
task :docs do
  files = Dir["src/*.erl"].map { |x| "'../" + x + "'"}.join " "
  sh %|cd doc && erl -noshell -s init stop -run edoc files #{files}|
end

desc "Starts textmate wit required directories only"
task :mate do
  sh "mate include src test ebin/flashpolicy.app Rakefile README.markdown"
end

