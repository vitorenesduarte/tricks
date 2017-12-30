import os
import time

def main():
    type = os.environ["TYPE"]
    if type == "hello-world":
        HelloWorld()
    else:
        print("Invalid type: " + type)

def HelloWorld():
    ###
    # Print N hello world messages:
    #  - N is given by env var COUNT 
    #  - sleep 2 seconds between messages
    ###
    count = int(os.environ["COUNT"])
    for i in xrange(count):
        print(str(i) + ") Hello World!")
        time.sleep(2)

if __name__ == "__main__":
    main()
