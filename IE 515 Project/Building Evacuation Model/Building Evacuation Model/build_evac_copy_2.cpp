// IE 515 - Graphs and Network Flows Project

#include <iostream>

using namespace std;


struct Arc
{
	int from;
	int to;
	int capacity;
	int cost;
	int flow;
};

struct GNode {
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
		int i = 1;
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

// end of linked list
int main()
{
	GNode nodes[6];
	int d[6] = { 0, 100000, 100000,  100000,  100000,  100000};
	bool labels[6] = { 0 ,0,0,0,0,1};

	for (int i = 0; i < 6; i++)
	{
		nodes[i].d = d[i];
		nodes[i].pred = 0;
		nodes[i].label = labels[i];
		nodes[i].node_index = i;
	}

	Arc arcs[9], reverse_arcs[9];
	
	int from[9] = { 1,1,2,4,3,3,4,5,5 };
	int to[9] = {   2,3,3,2,4,5,6,4,6 };
	int capacity[9] = { 20, 10, 5,  7,  23,  12,  3,  4,  8 };
	int cost[9] = { 6,4,2,-10,1,2,7,1,3};

	// Construction of arcs from the data
	for(int i = 0; i < 9; i++)
	{
		from[i] = from[i] - 1;
		to[i] = to[i] - 1;
		arcs[i].from = from[i];
		arcs[i].to = to[i];
		arcs[i].capacity = capacity[i];
		arcs[i].cost = cost[i];
		arcs[i].flow = 0;
	}

	for (int i = 0; i < 9; i++)
		cout << "From: " << arcs[i].from << " To: " << arcs[i].to << " Capacity: " << arcs[i].capacity << " Cost :" << arcs[i].cost << endl;


	
	list label_list, augment_list;

	//Beginning of max flow labeling algorithm
	while (nodes[5].label = 1)
	{
		//unlabel all
		for (int i = 0; i < 6; i++)
		{
			nodes[i].label = 0;
		}
		//label s
		nodes[0].label = 1;
		//set LIST
		label_list.createnode(0);

		//while(LIST is not empty
		while (label_list.length() > 1)
		{
			// keep element at j
			int j = label_list.get_element(0);
			// remove element from LIST
			label_list.delete_first();
			//for each arc emanating from the element 
			for (int i = 0; i < 9; i++)
			{
				//if to is not labeled, label and keep pred info and arc info
				if (arcs[i].from == j && nodes[arcs[i].to].label == 0 && arcs[i].capacity>0)
				{
					nodes[arcs[i].to].label = 1;
					nodes[arcs[i].to].pred = j;
					nodes[arcs[i].to].pred_arc = i;
					//add element to the list
					label_list.createnode(arcs[i].to);
				}
			}
		}
		/*
		//if t is labeled
		if (nodes[5].label == 1)
		{
			augment_list.createnode(5);

			//add preds to beginning of the list
			int j = nodes[5].pred;
			while (j != 0)
			{
				augment_list.insert_start(j);
				j = nodes[j].pred;
			}
			// add first node to the list
			augment_list.insert_start(0);

			int min = 100000;
			// find minimum capacity in the path
			for (int i = 0; i < augment_list.length(); i++)
			{
				if (arcs[nodes[augment_list.get_element(i)].pred_arc].capacity < min)
					min = arcs[nodes[augment_list.get_element(i)].pred_arc].capacity;
			}

			for (int i = 0; i < augment_list.length(); i++)
			{
				arcs[nodes[augment_list.get_element(i)].pred_arc].flow += min;
				arcs[nodes[augment_list.get_element(i)].pred_arc].capacity -= min;
			}
		}
		*/
	}

	cout << "Augmentation: " << endl;

	for (int i = 0; i < 9; i++)
		cout << "From: " << arcs[i].from << " To: " << arcs[i].to << " Capacity: " << arcs[i].capacity << " Cost :" << arcs[i].cost << endl;

	/*

	bool cycle_found = true;
	bool neg_cycle = false;
	while (neg_cycle == true)
	{

		bool done = false;
		int k = 0;
		neg_cycle = false;

		
		// Bellman Ford Main loop
		while (done == false)
		{
			done = true;
			for (int i = 0; i < 9; i++)
			{
				if (d[arcs[i].to] > d[arcs[i].from] + arcs[i].cost)
				{
					d[arcs[i].to] = d[arcs[i].from] + arcs[i].cost;
					pred[arcs[i].to] = arcs[i].from;
					pred_arcs[arcs[i].to] = i;
					done = false;
				}
			}

			k = k + 1;

			if (k >= 10)
			{
				done = true;
				neg_cycle = true;
			}
		}


		// Result output loop
		for (int i = 0; i < 6; i++)
		{
			cout << "Node i = " << i + 1 << "  -  Pred i = " << pred[i] + 1 << endl;
		}
		cout << "Negative Cycle : " << neg_cycle << endl;



		// Checks where is the negative cost cycle
		cycle_found = false;
		list negative_cycle;
		if (neg_cycle == true)
		{
			if (pred[0] != 0)
			{
				cout << "First Node in the cycle" << endl;
				int i = 0;
				int j = pred[i];
				while (i != j)
				{
					cout << "Node i: " << i + 1 << "Pred i" << j + 1 << endl;
				}
			}
			else if (pred[0] == 0)
			{
				cout << "node 1 is not in the cycle" << endl;

				int i = 1;
				int j = pred[i];
				while (cycle_found == false)
				{
					if (j != 0)
					{
						if (i == j)
						{

							cout << "Neg cycle exists" << endl;

							negative_cycle.createnode(i);
							j = pred[i];
							int k = i;
							while (k != j)
							{
								negative_cycle.createnode(j);
								i = j;
								j = pred[j];

							}

							cycle_found = true;
						}
						else
						{
							j = pred[j];
						}
					}
					else
					{
						i++;
						j = pred[i];
					}
				}
			}
		}

		negative_cycle.display();

		int i = 0;
		// w -> negative cycle arcs
		list w;

		while (negative_cycle.get_element(i) != NULL)
		{
			w.createnode(pred_arcs[negative_cycle.get_element(i)]);
			i++;
		}

		w.display();


		i = 0;

		while (w.get_element(i) != NULL)
		{
			int j = w.get_element(i);
			cout << "from : " << arcs[j].from << " to : " << arcs[j].to << endl;
			i++;
		}

		int min = 100000;

		for (i = 0; i < w.length(); i++)
		{
			if (arcs[w.get_element(i)].capacity < min)
				min = arcs[w.get_element(i)].capacity;
		}

		cout << "min = " << min << endl;

		//Augment

		for (i = 0; i < w.length(); i++)
		{
			arcs[w.get_element(i)].flow += min;
			arcs[w.get_element(i)].capacity -= min;
		}

	}
	*/
	
	
	char c;
	cout << "Press e to exit..." << endl;
	cin >> c;
	return 0;
}