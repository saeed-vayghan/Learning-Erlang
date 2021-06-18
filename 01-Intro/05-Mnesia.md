#### What is Mnesia good at?

* Locking and Transactions
	If you need to keep a database that will be used by multiple processes and/or nodes, using Mnesia means you don't have to write your own access controls. 

* Distribution (redundancy)
	Tables can be replicated at many nodes, both for efficiency and robustness.

* Non-fully-normalised data
	Unlike most database systems, records can contain data of arbitrary size and structure.

* Monitoring
	Monitoring processes can subscribe to events which are sent when various operations on the data take place (update, delete, etc).

<hr>

#### What is Mnesia not so good at?

* Mnesia is primarily intended to be a memory-resident database. Some of its design tradeoffs reflect this. 
* Is Mnesia good for storing blobs? It depends!
	* Available memory limitaiton.
  * disc_only_copies tables do not suffer from this limitation but they are slow.
  * Replication - if the table has a replica then updating an entry and rebuilding after a restart will copy the data over the network between the two machines.
* How much data can be stored in Mnesia?
  * For ram_copies and disc_copies, the entire table is kept in memory, so data size is limited by available RAM.
  * Note that for disc_copies tables, the entire table needs to be read from disk to memory on node startup.
  * disc_only_copies tables are limited to 2 GB each. If your table is fragmented, each fragment counts as a separate table, and the combined size can thus exceed 2 GB.
  * The reason for this limit is that disc_only_copies tables are backed by Dets tables, and the Dets file format uses signed 32-bit integers for file offsets.