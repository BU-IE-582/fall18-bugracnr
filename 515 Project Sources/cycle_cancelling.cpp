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
	GNode nodes[8];
	int d[8] = { 0, 100000, 100000,  100000,  100000,  100000, 100000 , 100000 };
	bool labels[8] = { 0 ,0,0,0,0,0,0,1 };

	Arc arcs[14], res_arcs[28];

	int from[14] = { 0,0,1,1,2,2,2,3,3,4,4,5,5,6 };
	int to[14] = { 1,2,2,3,3,4,5,4,5,5,6,6,7,7 };
	int capacity[14] = { 5,3,10,6,3,1,7,6,5,3,1,8,2,6 };
	int cost[14] = { 0,0,4,3,4,2,5,1,1,1,1,2,0,0 };
	int arc_no = 14,
		res_arc_no = 28,
		node_no = 8,
		last_node = 7;


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