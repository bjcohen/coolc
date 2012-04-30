set :application, "stanford-compilers"
set :repository,  "git@github.com:bjcohen/stanford-compilers.git"

INSTANCE_NAME = "i-76e5ed11"

public_dns = `ec2-describe-instances #{INSTANCE_NAME} | grep INSTANCE | awk '{print $4}'`.chomp

set :scm, :git

role :app, "ubuntu@#{public_dns}"

# namespace :deploy do
#   task :start do end
#   task :stop do end
#   task :restart do end
#   task :finalize_update do
#     run "chmod -R g+w #{current_release}"
#   end
#   task :build do
#     run "cd #{current_release} && make"
#   end
# end

# after 'deploy:finalize_update', 'deploy:build'
