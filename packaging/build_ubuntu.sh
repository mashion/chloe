basedir=$(dirname $0)
source $basedir/common.sh

host="${1:-"ubuntu@$(start_ubuntu_32_bit)"}"

ssh $host <<EOS
sudo apt-get -y update
sudo apt-get install -y build-essential libncurses5-dev openssl libssl-dev
sudo apt-get install -y erlang  # Installs R13B03, should probably install erlang / find better package
sudo apt-get install -y git-core rake gcc libpam0g-dev rubygems ruby-dev

# Get erlang
wget http://www.erlang.org/download/otp_src_R14B02.tar.gz
tar zxvf otp_src_R14B02.tar.gz
cd otp_src_R14B02
./configure && make && sudo make install
cd -

# Get rebar
git clone https://github.com/basho/rebar.git
cd rebar
escript ./bootstrap
sudo mv rebar /usr/local/bin
cd -

sudo gem install bundler --no-rdoc --no-ri
git clone https://github.com/mashion/chloe.git
cd chloe

# TODO (trotter): Autodetermine where bundle exec is
/var/lib/gems/1.8/bin/bundle install --local
rake bootstrap platform_release
EOS

scp $host:~/chloe/rel/chloe-*.tgz .
