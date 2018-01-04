import os
import time

def main():
    type = os.environ["TYPE"]
    if type == "hello-world":
        HelloWorld()
    elif type == "server":
        Server()
    elif type == "client":
        Client()
    else:
        print("Invalid type: " + type)

def HelloWorld():
    ###
    # Print N hello world messages:
    #  - N is given by env var COUNT 
    #  - sleep 2 seconds between messages
    ###
    count = int(os.environ["COUNT"])
    for i in range(count):
        print(str(i) + ") Hello World!")
        time.sleep(2)

def Server():
    ###
    # TODO
    ###
    while True:
        time.sleep(10)

def Client():
    ###
    # TODO
    ###
    ops = int(os.environ["OPS"])
    for i in range(ops):
        print(str(i) + ") OP!")
        time.sleep(.2)

if __name__ == "__main__":
    main()
