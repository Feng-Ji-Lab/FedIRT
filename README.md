# An example for Shiny App usage

Taking the 2PL as an example, we illustrate how to use the Shiny app below.

In the first step, the server end (e.g., test administer, school board) can be launched by running the Shiny app (`runserver()`) with the interface shown below:

![The initial server interface. \label{server1}](paper/server1.png)

Then, the client-end Shiny app can be initialized (`runclient()`). 

When the client first launches, it will automatically connect to the localhost port `8000` as default. 

If the server is deployed on another computer, type the server's IP address and port (which will be displayed on the server's interface), then click "reconnect". The screenshots of the user interface are shown below. 

![Server interface when one school is connected. \label{server2}](paper/server2.png)

![Client interface when connected to server. \label{client2}](paper/client2.png)

Then, the client should choose a file to upload to the local Shiny app to do local calculations, without sending it to the server. The file should be a `csv` file, with either binary or graded response, and all clients should share the same number of items, and the same maximum score in each item (if the answers are polytomous), otherwise, there will be an error message suggesting to check the datasets of all clients.

![Server iterface when one school uploaded dataset. \label{server3}](paper/server3.png)

![Client interface when a dataset is uploaded successfully. \label{client3}](paper/client3.png)

After all the clients upload their data, the server should click "start" to begin the federated estimates process and after the model converges, the client should click "receive result". The server will display all item parameters and the client will display all item parameters and individual ability estimates. 

![Server interface when estimation is completed. \label{server4}](paper/server4.png)

![Client interface when the results received. \label{client4}](paper/client4.png)

The clients will also display bar plots of the ability estimates. 

![Client interface for displaying results. \label{client5}](paper/client5.png)
