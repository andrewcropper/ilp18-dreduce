rm -rf programs
mkdir programs
trials=2
tasks=8

function learn {
  name=$1
  task=$2
  k=$3
  echo $name $task $k
    yap << % >> "programs/$name-$task-$k.pl"
        load_files(['data/train-$task-$k','experiment','rules-$name'],[silent(true)]).
        do_learn.
%
}

for k in `seq $trials`
do
  for t in `seq $tasks`
  do
    learn 'e' $t $k
    learn 'd' $t $k
    learn 'd2' $t $k
  done
done