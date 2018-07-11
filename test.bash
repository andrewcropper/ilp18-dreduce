rm -rf results
mkdir results

function f {
  name=$1
  task=$2
  k=$3
  echo $name $task $k
  yap -q << % >> "results/$name-$task-$k.txt"
  ['programs/$name-$task-$k'].
  ['data/test-$task-$k'].
  ['data/trains'].
  ['experiment'].
  do_test.
%
}

for k in `seq 20`
do
  for t in `seq 8`
  do
    f 'e' $t $k
    f 'd' $t $k
    f 'd2' $t $k
  done
done
