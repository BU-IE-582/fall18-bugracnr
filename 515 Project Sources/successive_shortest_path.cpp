
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

	void insert_position(int pos, int value)
	{
		node *pre = new node;
		node *cur = new node;
		node *temp = new node;
		cur = head;
		for (int i = 1; i<pos; i++)
		{
			pre = cur;
			cur = cur->next;
		}
		temp->data = value;
		pre->next = temp;
		temp->next = cur;
	}
	void delete_first()
	{
		node *temp = new node;
		temp = head;
		head = head->next;
		delete temp;
	}
	void delete_last()
	{
		node *current = new node;
		node *previous = new node;
		current = head;
		while (current->next != NULL)
		{
			previous = current;
			current = current->next;
		}
		tail = previous;
		previous->next = NULL;
		delete current;
	}
	void delete_position(int pos)
	{
		node *current = new node;
		node *previous = new node;
		current = head;
		for (int i = 1; i<pos; i++)
		{
			previous = current;
			current = current->next;
		}
		previous->next = current->next;
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
	GNode nodes[4];
	
	int b[4] = { 4,0,0,-4},
		from[5] = { 0,0,1,1,2},
		to[5] = { 1,2,2,3,3,},
		cap[5] = { 4,2,2,3,5 },
		cost[5] = { 2,2,1,3,1},
		arc_no = 5,
		res_arc_no,
		node_no = 4,
		last_node;
	res_arc_no = arc_no * 2;
	last_node = node_no - 1;

	Arc arcs[5], res_arcs[10];


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

	printf("Time taken: %.2fs\n", (double)(clock() - tStart) / CLOCKS_PER_SEC);


	char c;
	cout << "Press e to exit..." << endl;
	cin >> c;
	return 0;
}