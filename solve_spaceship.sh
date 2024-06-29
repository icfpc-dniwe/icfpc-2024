prefix="solve spaceship1 "
solution=$(echo spaceship/1.txt | python spaceship.py)
message=$(echo $prefix$solution | python encoder.py)
echo $message
echo $(echo $message | python interpreter.py)
echo $$message | python sender.py | python interpreter.py
