function start_instance() {
  instance_id=$(ec2-run-instances $@ | grep INSTANCE | cut -f2)
  host=""

  while [ -z "$host" ]
  do
    host=$(ec2-describe-instances | grep $instance_id | cut -f4)
    sleep 1
  done

  echo "$host"
}

function start_ubuntu_32_bit() {
  start_instance ami-a6f504cf -k trotter-personal-ec2
}

function start_ubuntu_64_bit() {
  start_instance ami-08f40561 -t m1.large -k trotter-personal-ec2
}
