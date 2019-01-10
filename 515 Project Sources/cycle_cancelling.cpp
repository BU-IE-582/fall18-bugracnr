// IE 515 - Graphs and Network Flows Project

#include <iostream>
#include <time.h>
using namespace std;


struct Arc
{
	int from;
	int to;
	int capacity;
	int cost;
	int flow;
	int mate;
};

struct GNode {
	int node_index;
	int d;
	int pred;
	int pred_arc;
	bool label;
};

// Linked List Code
struct node
{
	int data;
	node *next;
};

class list
{
private:
	node *head, *tail;
public:
	list()
	{
		head = NULL;
		tail = NULL;
	}
	void createnode(int value)
	{
		node *temp = new node;
		temp->data = value;
		temp->next = NULL;
		if (head == NULL)
		{
			head = temp;
			tail = temp;
			temp = NULL;
		}
		else
		{
			tail->next = temp;
			tail = temp;
		}
	}
	void display()
	{
		node *temp = new node;
		temp = head;
		while (temp != NULL)
		{
			cout << temp->data << "\t";
			temp = temp->next;
		}
		cout << endl;
	}
	
	int get_element(int n)
	{
		node *temp = new node;
		temp = head;
		int i = 0;
		while (i != n)
		{
			temp = temp->next;
			i++;
		}
		if (temp != NULL)
			return temp->data;
		else
			return NULL;
	}
	int length()
	{
		node *temp = new node;
		temp = head;
		int i = 0;
		while(temp != NULL)
		{
			temp = temp->next;
			i++;
		}
		return i;
	}
	void insert_start(int value)
	{
		node *temp = new node;
		temp->data = value;
		temp->next = head;
		head = temp;
	}

	
	void delete_first()
	{
		node *temp = new node;
		temp = head;
		head = head->next;
		delete temp;
	}
};


void augment(int min, Arc& a, Arc& b)
{
	a.capacity -= min;
	b.capacity += min;
	
}
// end of linked list
int main()
{
	GNode nodes[204];
	int d[204] = { 0,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000 };
	bool labels[204] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 };

	Arc arcs[306], res_arcs[306];

	int from[306] = { 2,2,3,3,3,4,5,7,7,8,8,8,9,10,12,12,13,13,13,14,15,17,17,18,18,18,19,20,22,22,23,23,23,24,25,27,27,28,28,28,29,30,32,32,33,33,33,34,35,37,37,38,38,38,39,40,42,42,43,43,43,44,45,47,47,48,48,48,49,50,52,52,53,53,53,54,55,57,57,58,58,58,59,60,62,62,63,63,63,64,65,67,67,68,68,68,69,70,72,72,73,73,73,74,75,77,77,78,78,78,79,80,82,82,83,83,83,84,85,87,87,88,88,88,89,90,92,92,93,93,93,94,95,97,97,98,98,98,99,100,102,102,103,103,103,104,105,107,107,108,108,108,109,110,112,112,113,113,113,114,115,117,117,118,118,118,119,120,122,122,123,123,123,124,125,127,127,128,128,128,129,130,132,132,133,133,133,134,135,137,137,138,138,138,139,140,142,142,143,143,143,144,145,147,147,148,148,148,149,150,152,152,153,153,153,154,155,157,157,158,158,158,159,160,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,81,86,91,96,101,106,111,116,121,126,131,136,141,146,151,156,161,166,171,176,181,186,191,196,201,0,202 };
	int to[306] = { 13,9,9,10,21,16,11,18,14,14,15,26,21,16,23,19,19,20,31,26,21,28,24,24,25,36,31,26,33,29,29,30,41,36,31,38,34,34,35,46,41,36,43,39,39,40,51,46,41,48,44,44,45,56,51,46,53,49,49,50,61,56,51,58,54,54,55,66,61,56,63,59,59,60,71,66,61,68,64,64,65,76,71,66,73,69,69,70,81,76,71,78,74,74,75,86,81,76,83,79,79,80,91,86,81,88,84,84,85,96,91,86,88,89,89,90,91,91,91,93,94,94,95,96,96,96,98,99,99,100,101,101,101,103,104,104,105,106,106,106,108,109,109,110,111,111,111,113,114,114,115,116,116,116,118,119,119,120,121,121,121,123,124,124,125,126,126,126,128,129,129,130,131,131,131,133,134,134,135,136,136,136,138,139,139,140,141,141,141,143,144,144,145,146,146,146,148,149,149,150,151,151,151,153,154,154,155,156,156,156,158,159,159,160,161,161,161,163,164,164,165,166,166,166,2,7,12,17,22,27,32,37,42,47,52,57,62,67,72,77,82,87,92,97,102,107,112,117,122,127,132,137,142,147,152,157,162,167,172,177,182,187,192,197,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,202,1,203 };
	int capacity[306] = { 10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,500,500 };
	int cost[306] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,10,20,40,80,160,320,640,1280,2560,5120,10240,20480,40960,81920,163840,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,0,0 };
	int arc_no = 306,
		res_arc_no = 612,
		node_no = 204,
		last_node = 203;

	for (int i = 0; i < node_no; i++)
	{
		nodes[i].d = d[i];
		nodes[i].pred = 0;
		nodes[i].label = labels[i];
		nodes[i].node_index = i;
	}

	

	
	


	// Construction of arcs from the data
	for(int i = 0; i < arc_no; i++)
	{
		from[i] = from[i] ;
		to[i] = to[i] ;
		arcs[i].from = from[i];
		arcs[i].to = to[i];
		arcs[i].capacity = capacity[i];
		arcs[i].cost = cost[i];
		arcs[i].mate = i + arc_no;
	}
	
	for (int i = 0; i < arc_no; i++)
		cout << "From: " << arcs[i].from -1 << " To: " << arcs[i].to -1 << " Capacity: " << arcs[i].capacity << " Cost :" << arcs[i].cost << endl;

	for (int i = 0; i < arc_no; i++)
		res_arcs[i] = arcs[i];

	for (int i = arc_no; i < res_arc_no; i++)
	{
		res_arcs[i].from = arcs[i-arc_no].to;
		res_arcs[i].to = arcs[i - arc_no].from;
		res_arcs[i].capacity = 0;
		res_arcs[i].cost = -arcs[i - arc_no].cost;
		res_arcs[i].mate = i - arc_no;

	}


	clock_t tStart = clock();

	list label_list, augment_list;

	//Beginning of max flow labeling algorithm
	while (nodes[last_node].label == 1)
	{
		//unlabel all
		for (int i = 0; i < node_no; i++)
		{
			nodes[i].label = 0;
			nodes[i].pred = 0;
		}
		//label s
		nodes[0].label = 1;
		//set LIST
		label_list.createnode(0);

		//while(LIST is not empty
		while (label_list.length() > 0)
		{
			// keep element at j
			int j = label_list.get_element(0);
			// remove element from LIST
			label_list.delete_first();
			//for each arc emanating from the element 
			for (int i = 0; i < res_arc_no; i++)
			{
				//if to is not labeled, label and keep pred info and arc info
				if (res_arcs[i].from == j && nodes[res_arcs[i].to].label == 0 && res_arcs[i].capacity>0)
				{
					nodes[res_arcs[i].to].label = 1;
					nodes[res_arcs[i].to].pred = res_arcs[i].from;
					nodes[res_arcs[i].to].pred_arc = i;
					//add element to the list
					label_list.createnode(res_arcs[i].to);
				}
			}
		}
		//if t is labeled
		if (nodes[last_node].label == 1)
		{
			augment_list.createnode(last_node);

			//add preds to beginning of the list
			int j = nodes[last_node].pred;
			while (j != 0)
			{
				augment_list.insert_start(j);
				j = nodes[j].pred;
			}
			// add first node to the list
			cout << augment_list.length() << endl;
			augment_list.display();

			int min = 100000;
			// find minimum capacity in the path
			for (int i = 0; i < augment_list.length(); i++)
			{
				int b = augment_list.get_element(i);
				int a = nodes[b].pred_arc;
				int k = res_arcs[a].capacity;
				if (k < min)
					min = k;
			}


			for (int i = 0; i < augment_list.length(); i++)
			{
				augment(min, res_arcs[nodes[augment_list.get_element(i)].pred_arc], res_arcs[res_arcs[nodes[augment_list.get_element(i)].pred_arc].mate]);
			}
			int length = augment_list.length();
			for (int i = 0; i < length; i++)
			{
				augment_list.delete_first();
			}

		
			for (int i = 0; i < res_arc_no; i++)
				cout << "*** Augmented: From: " << res_arcs[i].from << " To: " << res_arcs[i].to << " Capacity: " << res_arcs[i].capacity << " Cost :" << res_arcs[i].cost << endl;

		}
	}

	cout << "Last Round: Augmentation: " << endl;

	for (int i = 0; i < arc_no; i++)
		cout << "From: " << res_arcs[i].from << " To: " << res_arcs[i].to << " Flow: " << arcs[i].capacity - res_arcs[i].capacity << " Cost :" << res_arcs[i].cost  << endl;
	
	
	

	///*
	bool neg_cycle = true;
	
	while (neg_cycle == true)
	{

		bool done = false;
		int k = 0;
		neg_cycle = false;

		for (int i = 0; i < node_no; i++)
		{
			nodes[i].d = 1000000;
			nodes[i].pred = 0;
			nodes[i].pred_arc = 0;
			nodes[i].label = 0;
		}

		nodes[0].d = 0;
				
		// Bellman Ford Main loop
		while (done == false)
		{
			done = true;
			for (int i = 0; i < res_arc_no; i++)
			{
				if (nodes[res_arcs[i].to].d > nodes[res_arcs[i].from].d + res_arcs[i].cost && res_arcs[i].capacity>0)
				{
					nodes[res_arcs[i].to].d = nodes[res_arcs[i].from].d + res_arcs[i].cost;
					nodes[res_arcs[i].to].pred = res_arcs[i].from;
					nodes[res_arcs[i].to].pred_arc = i;
					nodes[res_arcs[i].to].label = 1;

					done = false;
				}
			}

			k = k + 1;

			if (k >= node_no)
			{
				done = true;
				neg_cycle = true;
			}
		}


		// Result output loop
		for (int i = 0; i < node_no; i++)
		{
			cout << "Node i = " << i  << "  -  Pred i = " << nodes[i].pred << 
			" dist: "	<< nodes[i].d << " Pred arc = " << nodes[i].pred_arc <<  endl;
		}
		cout << "Negative Cycle : " << neg_cycle << endl;



		// Checks where is the negative cost cycle
		bool cycle_found = false;
		list negative_cycle;
		if (neg_cycle == true)
		{
			int i = 0;
			int c = 0;
			while (cycle_found == false)
			{
				if (nodes[i].pred == i)
				{
					i++;
				}

				int j = nodes[i].pred;
				
				while (j != i & c <= node_no)
				{
					j = nodes[j].pred;
					c++;
				}
				if (i != j)
				{
					i++;
					c = 0;
				}
				else
				{
					negative_cycle.createnode(i);
					j = nodes[i].pred;
					while (i != j)
					{
						negative_cycle.createnode(j);
						j = nodes[j].pred;
					}
					cycle_found = true;
				}


			}
		}

		negative_cycle.display();

		int i = 0;
		// w -> negative cycle res_arcs
		list w;

		for(int i = 0; i < negative_cycle.length(); i++ )
		{
			w.createnode(nodes[negative_cycle.get_element(i)].pred_arc);
			
		}

		w.display();


	
		for(int i = 0; i < w.length(); i++)
		{
			int j = w.get_element(i);
			cout << "from : " << res_arcs[j].from << " to : " << res_arcs[j].to << endl;
			
		}

		int min = 100000;

		for (i = 0; i < w.length(); i++)
		{
			if (res_arcs[w.get_element(i)].capacity < min)
				min = res_arcs[w.get_element(i)].capacity;
		}

		cout << "min = " << min << endl;

		//Augment

		for (i = 0; i < w.length(); i++)
		{
			augment(min, res_arcs[w.get_element(i)], res_arcs[res_arcs[w.get_element(i)].mate]);
		}

		cout << "Bellman: Augmentation: " << endl;

		for (int i = 0; i < res_arc_no; i++)
			cout << "From: " << res_arcs[i].from << " To: " << res_arcs[i].to << " Capacity: " << res_arcs[i].capacity << " Cost :" << res_arcs[i].cost << endl;


	}
	//*/
	
	cout << "Final Network: " << endl;

	for (int i = 0; i < arc_no; i++)
		cout << "From: " << res_arcs[i].from - 1 << " To: " << res_arcs[i].to -1 << " flow: " << arcs[i].capacity - res_arcs[i].capacity << " Cost :" << res_arcs[i].cost << endl;

	cout << "n: " << node_no << " m: " << arc_no << "  ";

	printf("Time taken: %.2fs\n", (double)(clock() - tStart) / CLOCKS_PER_SEC);

	
	char c;
	cout << "Press e to exit..." << endl;
	cin >> c;
	return 0;
}