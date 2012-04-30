INSTANCE_NAME = "i-76e5ed11"

public_dns = `ec2-describe-instances #{INSTANCE_NAME} | grep INSTANCE | awk '{print $4}'`.chomp

namespace :aws do
  task :start do
    puts `ec2start #{INSTANCE_NAME}`
  end
  task :stop do
    puts `ec2stop #{INSTANCE_NAME}`
  end
  task :restart do
    puts `ec2reboot #{INSTANCE_NAME}`
  end
  task :query do
    puts public_dns
  end
  task :invoke do
    run ENV['COMMAND']
  end
end

