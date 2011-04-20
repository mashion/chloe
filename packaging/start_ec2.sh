instance_id=$(ec2-run-instances ami-a6f504cf -f sample_user_data.sh | grep INSTANCE | cut -f2)
host=""

while [ -z "$host" ]
do
  host=$(ec2-describe-instances | grep $instance_id | cut -f4)
  sleep 1
done

echo "Host is $host"
