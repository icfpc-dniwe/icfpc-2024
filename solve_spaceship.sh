number=$1
prefix="solve spaceship$number "
solution=$(echo spaceship/$number.txt | python spaceship.py)
message=$(echo $prefix$solution | python encoder.py)
echo $message
echo $(echo $message | python interpreter.py)
echo $$message | python sender.py | python interpreter.py
