
#include <iostream>
#include <time.h>
using namespace std;


struct Arc
{
	int from;
	int to;
	int capacity;
	int cost;
	int reduced_cost;
	int flow;
	int mate;
};

struct GNode {
	int d;
	int pred;
	int potential;
	int b;
	int imbalance;
	int pred_arc;
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
		while (temp != NULL)
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


void augment(int min, int index, int limit, Arc& a, Arc& b)
{
	a.capacity -= min;
	b.capacity += min;
	if (index < limit)
		a.flow += min;
	else
		b.flow -= min;
}

int main()
{
	GNode nodes[202];

	int b[202] = { 500,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-500 },
		from[304] = { 1,1,2,2,2,3,4,6,6,7,7,7,8,9,11,11,12,12,12,13,14,16,16,17,17,17,18,19,21,21,22,22,22,23,24,26,26,27,27,27,28,29,31,31,32,32,32,33,34,36,36,37,37,37,38,39,41,41,42,42,42,43,44,46,46,47,47,47,48,49,51,51,52,52,52,53,54,56,56,57,57,57,58,59,61,61,62,62,62,63,64,66,66,67,67,67,68,69,71,71,72,72,72,73,74,76,76,77,77,77,78,79,81,81,82,82,82,83,84,86,86,87,87,87,88,89,91,91,92,92,92,93,94,96,96,97,97,97,98,99,101,101,102,102,102,103,104,106,106,107,107,107,108,109,111,111,112,112,112,113,114,116,116,117,117,117,118,119,121,121,122,122,122,123,124,126,126,127,127,127,128,129,131,131,132,132,132,133,134,136,136,137,137,137,138,139,141,141,142,142,142,143,144,146,146,147,147,147,148,149,151,151,152,152,152,153,154,156,156,157,157,157,158,159,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200 },
		to[304] = { 12,8,8,9,20,15,10,17,13,13,14,25,20,15,22,18,18,19,30,25,20,27,23,23,24,35,30,25,32,28,28,29,40,35,30,37,33,33,34,45,40,35,42,38,38,39,50,45,40,47,43,43,44,55,50,45,52,48,48,49,60,55,50,57,53,53,54,65,60,55,62,58,58,59,70,65,60,67,63,63,64,75,70,65,72,68,68,69,80,75,70,77,73,73,74,85,80,75,82,78,78,79,90,85,80,87,83,83,84,95,90,85,87,88,88,89,90,90,90,92,93,93,94,95,95,95,97,98,98,99,100,100,100,102,103,103,104,105,105,105,107,108,108,109,110,110,110,112,113,113,114,115,115,115,117,118,118,119,120,120,120,122,123,123,124,125,125,125,127,128,128,129,130,130,130,132,133,133,134,135,135,135,137,138,138,139,140,140,140,142,143,143,144,145,145,145,147,148,148,149,150,150,150,152,153,153,154,155,155,155,157,158,158,159,160,160,160,162,163,163,164,165,165,165,1,6,11,16,21,26,31,36,41,46,51,56,61,66,71,76,81,86,91,96,101,106,111,116,121,126,131,136,141,146,151,156,161,166,171,176,181,186,191,196,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201,201 },
		cap[304] = { 10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10,20,5,15,5,8,12,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000,10000 },
		cost[304] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5,10,20,40,80,160,320,640,1280,2560,5120,10240,20480,40960,81920,163840,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680,327680 },
		arc_no = 304,
		res_arc_no = 608,
		node_no = 202,
		last_node = 201;
	res_arc_no = arc_no * 2;
	last_node = node_no - 1;

	Arc arcs[304], res_arcs[608];


	// Construction of arcs from the data
	for (int i = 0; i < arc_no; i++)
	{
		arcs[i].from = from[i];
		arcs[i].to = to[i];
		arcs[i].capacity = cap[i];
		arcs[i].cost = cost[i];
		arcs[i].reduced_cost = cost[i];
		arcs[i].mate = i + arc_no;
		arcs[i].flow = 0;
	}

	for (int i = 0; i < arc_no; i++)
		cout << "From: " << arcs[i].from << " To: " << arcs[i].to << " Capacity: " << arcs[i].capacity << " Cost :" << arcs[i].cost << endl;

	//construction of residual net
	for (int i = 0; i < arc_no; i++)
		res_arcs[i] = arcs[i];

	for (int i = arc_no; i < res_arc_no; i++)
	{
		res_arcs[i].from = arcs[i - arc_no].to;
		res_arcs[i].to = arcs[i - arc_no].from;
		res_arcs[i].capacity = 0;
		res_arcs[i].cost = -arcs[i - arc_no].cost;
		res_arcs[i].reduced_cost = -arcs[i - arc_no].cost;
		res_arcs[i].mate = i - arc_no;
		res_arcs[i].flow = 0;
	}

	// initialization of imbalance and node potentials
	for (int i = 0; i < node_no; i++)
	{
		nodes[i].b = b[i];
		nodes[i].imbalance = nodes[i].b;
		nodes[i].potential = 0;
	}

	// initialization of excess and deficit lists
	list excess, deficit;

	for (int i = 0; i < node_no; i++)
	{
		if (nodes[i].imbalance > 0)
			excess.createnode(i);
		else if (nodes[i].imbalance < 0)
			deficit.createnode(i);
	}

	
	clock_t tStart = clock();

	int cou = 0;

	// main loop
	while (excess.length() > 0)
	{
		cout << "excess   ";
		excess.display();
		cout << "deficit  ";
		deficit.display();

		int k = excess.get_element(0);
		int l = deficit.get_element(0);

		cout << "k & l " << k << "   -   " << l << endl;

		// shortest path by bellman ford
		bool done = false;
		
		for (int i = 0; i < node_no; i++)
		{
			nodes[i].d = 1000000;
			nodes[i].pred = 0;
			nodes[i].pred_arc = 0;
		}

		nodes[k].d = 0;

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
					done = false;
				}
			}
		}

		list shortest_path, shortest_arcs;

		shortest_path.createnode(l);
		int pred_ = nodes[l].pred;
		while (pred_ != k)
		{
			shortest_path.insert_start(pred_);
			pred_ = nodes[pred_].pred;
		}

		shortest_path.insert_start(k);
		
		cout << "shortest path : "; shortest_path.display();

		for (int i = 0; i < shortest_path.length(); i++)
		{
			nodes[shortest_path.get_element(i)].potential = nodes[shortest_path.get_element(i)].potential - nodes[shortest_path.get_element(i)].d;

			if (i != 0)
				shortest_arcs.createnode(nodes[shortest_path.get_element(i)].pred_arc);
		}


		cout << "shortest arcs   ";
		shortest_arcs.display();

		int min = nodes[k].imbalance;

		if (min > -nodes[l].imbalance)
			min = -nodes[l].imbalance;

		for (int i = 0; i < shortest_arcs.length(); i++)
		{
			if (min > res_arcs[shortest_arcs.get_element(i)].capacity)
				min = res_arcs[shortest_arcs.get_element(i)].capacity;
		}

		cout << "augmented units  : " << min << endl;

		for (int i = 0; i < shortest_arcs.length(); i++)
		{
			augment(min, shortest_arcs.get_element(i), arc_no, res_arcs[shortest_arcs.get_element(i)], res_arcs[res_arcs[shortest_arcs.get_element(i)].mate]);
		}

		for (int i = 0; i < shortest_arcs.length(); i++)
		{
			nodes[res_arcs[shortest_arcs.get_element(i)].from].imbalance -= min;
		}

		for (int i = 0; i < shortest_arcs.length(); i++)
		{
			nodes[res_arcs[shortest_arcs.get_element(i)].to].imbalance += min;
		}
		
	
		for (int i = 0; i <= excess.length(); i++)
		{
			excess.delete_first();
		}
		for (int i = 0; i <= deficit.length(); i++)
		{
			deficit.delete_first();
		}

		for (int i = 0; i < node_no; i++)
		{
			if (nodes[i].imbalance > 0)
				excess.createnode(i);
			else if (nodes[i].imbalance < 0)
				deficit.createnode(i);
		}

		for (int i = 0; i < res_arc_no; i++)
		{
			res_arcs[i].reduced_cost = res_arcs[i].cost - nodes[res_arcs[i].from].potential + nodes[res_arcs[i].to].potential;
		}

	

		for(int i = 0; i < node_no; i++)
			cout << "Node i = " << i << "  -  Pred i = " << nodes[i].pred <<
			" dist: " << nodes[i].d << " Pred arc = " << nodes[i].pred_arc 
			<< "   imbalance :  " << nodes[i].imbalance << endl;
	
		cout << "End of Tour : " << cou << endl;
		cou++;

}

	for (int i = 0; i < res_arc_no; i++)
		cout << "From: " << res_arcs[i].from << " To: " << res_arcs[i].to << " Flow: " << res_arcs[i].flow << " Capacity: " << res_arcs[i].capacity << " Cost :" << res_arcs[i].cost << endl;

	cout << "Final Network: " << endl;

	for (int i = 0; i < arc_no; i++)
		cout << "From: " << res_arcs[i].from << " To: " << res_arcs[i].to << " flow: " << arcs[i].capacity - res_arcs[i].capacity << " Cost :" << res_arcs[i].cost << endl;

	cout << "n: " << node_no << " m: " << arc_no << "  ";

	printf("Time taken: %.2fs\n", (double)(clock() - tStart) / CLOCKS_PER_SEC);

	char c;
	cout << "Press e to exit..." << endl;
	cin >> c;
	return 0;
}