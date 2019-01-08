// IE 515 - Graphs and Network Flows Project

#include <iostream>
#include<vector>
#include<random>

using namespace std;


struct Arc
{
	int from;
	int to;
	int capacity;
	int cost;
};



int main()
{
	Arc arcs[152];
	
	arcs[0].from = 1; arcs[0].to = 22; arcs[0].capacity = 10; arcs[0].cost = 0;
	arcs[1].from = 1; arcs[1].to = 13; arcs[1].capacity = 20; arcs[1].cost = 0;
	arcs[2].from = 2; arcs[2].to = 18; arcs[2].capacity = 5; arcs[2].cost = 0;
	arcs[3].from = 2; arcs[3].to = 9; arcs[3].capacity = 15; arcs[3].cost = 0;
	arcs[4].from = 2; arcs[4].to = 20; arcs[4].capacity = 5; arcs[4].cost = 0;
	arcs[5].from = 3; arcs[5].to = 25; arcs[5].capacity = 8; arcs[5].cost = 0;
	arcs[6].from = 4; arcs[6].to = 10; arcs[6].capacity = 12; arcs[6].cost = 0;
	arcs[7].from = 6; arcs[7].to = 27; arcs[7].capacity = 10; arcs[7].cost = 0;
	arcs[8].from = 6; arcs[8].to = 18; arcs[8].capacity = 20; arcs[8].cost = 0;
	arcs[9].from = 7; arcs[9].to = 23; arcs[9].capacity = 5; arcs[9].cost = 0;
	arcs[10].from = 7; arcs[10].to = 14; arcs[10].capacity = 15; arcs[10].cost = 0;
	arcs[11].from = 7; arcs[11].to = 25; arcs[11].capacity = 5; arcs[11].cost = 0;
	arcs[12].from = 8; arcs[12].to = 30; arcs[12].capacity = 8; arcs[12].cost = 0;
	arcs[13].from = 9; arcs[13].to = 15; arcs[13].capacity = 12; arcs[13].cost = 0;
	arcs[14].from = 11; arcs[14].to = 32; arcs[14].capacity = 10; arcs[14].cost = 0;
	arcs[15].from = 11; arcs[15].to = 23; arcs[15].capacity = 20; arcs[15].cost = 0;
	arcs[16].from = 12; arcs[16].to = 28; arcs[16].capacity = 5; arcs[16].cost = 0;
	arcs[17].from = 12; arcs[17].to = 19; arcs[17].capacity = 15; arcs[17].cost = 0;
	arcs[18].from = 12; arcs[18].to = 30; arcs[18].capacity = 5; arcs[18].cost = 0;
	arcs[19].from = 13; arcs[19].to = 35; arcs[19].capacity = 8; arcs[19].cost = 0;
	arcs[20].from = 14; arcs[20].to = 20; arcs[20].capacity = 12; arcs[20].cost = 0;
	arcs[21].from = 16; arcs[21].to = 37; arcs[21].capacity = 10; arcs[21].cost = 0;
	arcs[22].from = 16; arcs[22].to = 28; arcs[22].capacity = 20; arcs[22].cost = 0;
	arcs[23].from = 17; arcs[23].to = 33; arcs[23].capacity = 5; arcs[23].cost = 0;
	arcs[24].from = 17; arcs[24].to = 24; arcs[24].capacity = 15; arcs[24].cost = 0;
	arcs[25].from = 17; arcs[25].to = 35; arcs[25].capacity = 5; arcs[25].cost = 0;
	arcs[26].from = 18; arcs[26].to = 40; arcs[26].capacity = 8; arcs[26].cost = 0;
	arcs[27].from = 19; arcs[27].to = 25; arcs[27].capacity = 12; arcs[27].cost = 0;
	arcs[28].from = 21; arcs[28].to = 42; arcs[28].capacity = 10; arcs[28].cost = 0;
	arcs[29].from = 21; arcs[29].to = 33; arcs[29].capacity = 20; arcs[29].cost = 0;
	arcs[30].from = 22; arcs[30].to = 38; arcs[30].capacity = 5; arcs[30].cost = 0;
	arcs[31].from = 22; arcs[31].to = 29; arcs[31].capacity = 15; arcs[31].cost = 0;
	arcs[32].from = 22; arcs[32].to = 40; arcs[32].capacity = 5; arcs[32].cost = 0;
	arcs[33].from = 23; arcs[33].to = 45; arcs[33].capacity = 8; arcs[33].cost = 0;
	arcs[34].from = 24; arcs[34].to = 30; arcs[34].capacity = 12; arcs[34].cost = 0;
	arcs[35].from = 26; arcs[35].to = 47; arcs[35].capacity = 10; arcs[35].cost = 0;
	arcs[36].from = 26; arcs[36].to = 38; arcs[36].capacity = 20; arcs[36].cost = 0;
	arcs[37].from = 27; arcs[37].to = 43; arcs[37].capacity = 5; arcs[37].cost = 0;
	arcs[38].from = 27; arcs[38].to = 34; arcs[38].capacity = 15; arcs[38].cost = 0;
	arcs[39].from = 27; arcs[39].to = 45; arcs[39].capacity = 5; arcs[39].cost = 0;
	arcs[40].from = 28; arcs[40].to = 50; arcs[40].capacity = 8; arcs[40].cost = 0;
	arcs[41].from = 29; arcs[41].to = 35; arcs[41].capacity = 12; arcs[41].cost = 0;
	arcs[42].from = 31; arcs[42].to = 52; arcs[42].capacity = 10; arcs[42].cost = 0;
	arcs[43].from = 31; arcs[43].to = 43; arcs[43].capacity = 20; arcs[43].cost = 0;
	arcs[44].from = 32; arcs[44].to = 48; arcs[44].capacity = 5; arcs[44].cost = 0;
	arcs[45].from = 32; arcs[45].to = 39; arcs[45].capacity = 15; arcs[45].cost = 0;
	arcs[46].from = 32; arcs[46].to = 50; arcs[46].capacity = 5; arcs[46].cost = 0;
	arcs[47].from = 33; arcs[47].to = 55; arcs[47].capacity = 8; arcs[47].cost = 0;
	arcs[48].from = 34; arcs[48].to = 40; arcs[48].capacity = 12; arcs[48].cost = 0;
	arcs[49].from = 36; arcs[49].to = 57; arcs[49].capacity = 10; arcs[49].cost = 0;
	arcs[50].from = 36; arcs[50].to = 48; arcs[50].capacity = 20; arcs[50].cost = 0;
	arcs[51].from = 37; arcs[51].to = 53; arcs[51].capacity = 5; arcs[51].cost = 0;
	arcs[52].from = 37; arcs[52].to = 44; arcs[52].capacity = 15; arcs[52].cost = 0;
	arcs[53].from = 37; arcs[53].to = 55; arcs[53].capacity = 5; arcs[53].cost = 0;
	arcs[54].from = 38; arcs[54].to = 60; arcs[54].capacity = 8; arcs[54].cost = 0;
	arcs[55].from = 39; arcs[55].to = 45; arcs[55].capacity = 12; arcs[55].cost = 0;
	arcs[56].from = 41; arcs[56].to = 62; arcs[56].capacity = 10; arcs[56].cost = 0;
	arcs[57].from = 41; arcs[57].to = 53; arcs[57].capacity = 20; arcs[57].cost = 0;
	arcs[58].from = 42; arcs[58].to = 58; arcs[58].capacity = 5; arcs[58].cost = 0;
	arcs[59].from = 42; arcs[59].to = 49; arcs[59].capacity = 15; arcs[59].cost = 0;
	arcs[60].from = 42; arcs[60].to = 60; arcs[60].capacity = 5; arcs[60].cost = 0;
	arcs[61].from = 43; arcs[61].to = 65; arcs[61].capacity = 8; arcs[61].cost = 0;
	arcs[62].from = 44; arcs[62].to = 50; arcs[62].capacity = 12; arcs[62].cost = 0;
	arcs[63].from = 46; arcs[63].to = 67; arcs[63].capacity = 10; arcs[63].cost = 0;
	arcs[64].from = 46; arcs[64].to = 58; arcs[64].capacity = 20; arcs[64].cost = 0;
	arcs[65].from = 47; arcs[65].to = 63; arcs[65].capacity = 5; arcs[65].cost = 0;
	arcs[66].from = 47; arcs[66].to = 54; arcs[66].capacity = 15; arcs[66].cost = 0;
	arcs[67].from = 47; arcs[67].to = 65; arcs[67].capacity = 5; arcs[67].cost = 0;
	arcs[68].from = 48; arcs[68].to = 70; arcs[68].capacity = 8; arcs[68].cost = 0;
	arcs[69].from = 49; arcs[69].to = 55; arcs[69].capacity = 12; arcs[69].cost = 0;
	arcs[70].from = 51; arcs[70].to = 72; arcs[70].capacity = 10; arcs[70].cost = 0;
	arcs[71].from = 51; arcs[71].to = 63; arcs[71].capacity = 20; arcs[71].cost = 0;
	arcs[72].from = 52; arcs[72].to = 68; arcs[72].capacity = 5; arcs[72].cost = 0;
	arcs[73].from = 52; arcs[73].to = 59; arcs[73].capacity = 15; arcs[73].cost = 0;
	arcs[74].from = 52; arcs[74].to = 70; arcs[74].capacity = 5; arcs[74].cost = 0;
	arcs[75].from = 53; arcs[75].to = 75; arcs[75].capacity = 8; arcs[75].cost = 0;
	arcs[76].from = 54; arcs[76].to = 60; arcs[76].capacity = 12; arcs[76].cost = 0;
	arcs[77].from = 56; arcs[77].to = 77; arcs[77].capacity = 10; arcs[77].cost = 0;
	arcs[78].from = 56; arcs[78].to = 68; arcs[78].capacity = 20; arcs[78].cost = 0;
	arcs[79].from = 57; arcs[79].to = 73; arcs[79].capacity = 5; arcs[79].cost = 0;
	arcs[80].from = 57; arcs[80].to = 64; arcs[80].capacity = 15; arcs[80].cost = 0;
	arcs[81].from = 57; arcs[81].to = 75; arcs[81].capacity = 5; arcs[81].cost = 0;
	arcs[82].from = 58; arcs[82].to = 80; arcs[82].capacity = 8; arcs[82].cost = 0;
	arcs[83].from = 59; arcs[83].to = 65; arcs[83].capacity = 12; arcs[83].cost = 0;
	arcs[84].from = 61; arcs[84].to = 82; arcs[84].capacity = 10; arcs[84].cost = 0;
	arcs[85].from = 61; arcs[85].to = 73; arcs[85].capacity = 20; arcs[85].cost = 0;
	arcs[86].from = 62; arcs[86].to = 78; arcs[86].capacity = 5; arcs[86].cost = 0;
	arcs[87].from = 62; arcs[87].to = 69; arcs[87].capacity = 15; arcs[87].cost = 0;
	arcs[88].from = 62; arcs[88].to = 80; arcs[88].capacity = 5; arcs[88].cost = 0;
	arcs[89].from = 63; arcs[89].to = 85; arcs[89].capacity = 8; arcs[89].cost = 0;
	arcs[90].from = 64; arcs[90].to = 70; arcs[90].capacity = 12; arcs[90].cost = 0;
	arcs[91].from = 66; arcs[91].to = 87; arcs[91].capacity = 10; arcs[91].cost = 0;
	arcs[92].from = 66; arcs[92].to = 78; arcs[92].capacity = 20; arcs[92].cost = 0;
	arcs[93].from = 67; arcs[93].to = 83; arcs[93].capacity = 5; arcs[93].cost = 0;
	arcs[94].from = 67; arcs[94].to = 74; arcs[94].capacity = 15; arcs[94].cost = 0;
	arcs[95].from = 67; arcs[95].to = 85; arcs[95].capacity = 5; arcs[95].cost = 0;
	arcs[96].from = 68; arcs[96].to = 90; arcs[96].capacity = 8; arcs[96].cost = 0;
	arcs[97].from = 69; arcs[97].to = 75; arcs[97].capacity = 12; arcs[97].cost = 0;
	arcs[98].from = 71; arcs[98].to = 92; arcs[98].capacity = 10; arcs[98].cost = 0;
	arcs[99].from = 71; arcs[99].to = 83; arcs[99].capacity = 20; arcs[99].cost = 0;
	arcs[100].from = 72; arcs[100].to = 88; arcs[100].capacity = 5; arcs[100].cost = 0;
	arcs[101].from = 72; arcs[101].to = 79; arcs[101].capacity = 15; arcs[101].cost = 0;
	arcs[102].from = 72; arcs[102].to = 90; arcs[102].capacity = 5; arcs[102].cost = 0;
	arcs[103].from = 73; arcs[103].to = 95; arcs[103].capacity = 8; arcs[103].cost = 0;
	arcs[104].from = 74; arcs[104].to = 80; arcs[104].capacity = 12; arcs[104].cost = 0;
	arcs[105].from = 76; arcs[105].to = 97; arcs[105].capacity = 10; arcs[105].cost = 0;
	arcs[106].from = 76; arcs[106].to = 88; arcs[106].capacity = 20; arcs[106].cost = 0;
	arcs[107].from = 77; arcs[107].to = 93; arcs[107].capacity = 5; arcs[107].cost = 0;
	arcs[108].from = 77; arcs[108].to = 84; arcs[108].capacity = 15; arcs[108].cost = 0;
	arcs[109].from = 77; arcs[109].to = 95; arcs[109].capacity = 5; arcs[109].cost = 0;
	arcs[110].from = 78; arcs[110].to = 100; arcs[110].capacity = 8; arcs[110].cost = 0;
	arcs[111].from = 79; arcs[111].to = 85; arcs[111].capacity = 12; arcs[111].cost = 0;
	arcs[112].from = 0; arcs[112].to = 1; arcs[112].capacity = 10000; arcs[112].cost = 0;
	arcs[113].from = 0; arcs[113].to = 6; arcs[113].capacity = 10000; arcs[113].cost = 0;
	arcs[114].from = 0; arcs[114].to = 11; arcs[114].capacity = 10000; arcs[114].cost = 0;
	arcs[115].from = 0; arcs[115].to = 16; arcs[115].capacity = 10000; arcs[115].cost = 0;
	arcs[116].from = 0; arcs[116].to = 21; arcs[116].capacity = 10000; arcs[116].cost = 0;
	arcs[117].from = 0; arcs[117].to = 26; arcs[117].capacity = 10000; arcs[117].cost = 0;
	arcs[118].from = 0; arcs[118].to = 31; arcs[118].capacity = 10000; arcs[118].cost = 0;
	arcs[119].from = 0; arcs[119].to = 36; arcs[119].capacity = 10000; arcs[119].cost = 0;
	arcs[120].from = 0; arcs[120].to = 41; arcs[120].capacity = 10000; arcs[120].cost = 0;
	arcs[121].from = 0; arcs[121].to = 46; arcs[121].capacity = 10000; arcs[121].cost = 0;
	arcs[122].from = 0; arcs[122].to = 51; arcs[122].capacity = 10000; arcs[122].cost = 0;
	arcs[123].from = 0; arcs[123].to = 56; arcs[123].capacity = 10000; arcs[123].cost = 0;
	arcs[124].from = 0; arcs[124].to = 61; arcs[124].capacity = 10000; arcs[124].cost = 0;
	arcs[125].from = 0; arcs[125].to = 66; arcs[125].capacity = 10000; arcs[125].cost = 0;
	arcs[126].from = 0; arcs[126].to = 71; arcs[126].capacity = 10000; arcs[126].cost = 0;
	arcs[127].from = 0; arcs[127].to = 76; arcs[127].capacity = 10000; arcs[127].cost = 0;
	arcs[128].from = 0; arcs[128].to = 81; arcs[128].capacity = 10000; arcs[128].cost = 0;
	arcs[129].from = 0; arcs[129].to = 86; arcs[129].capacity = 10000; arcs[129].cost = 0;
	arcs[130].from = 0; arcs[130].to = 91; arcs[130].capacity = 10000; arcs[130].cost = 0;
	arcs[131].from = 0; arcs[131].to = 96; arcs[131].capacity = 10000; arcs[131].cost = 0;
	arcs[132].from = 5; arcs[132].to = 101; arcs[132].capacity = 10000; arcs[132].cost = 5;
	arcs[133].from = 10; arcs[133].to = 101; arcs[133].capacity = 10000; arcs[133].cost = 10;
	arcs[134].from = 15; arcs[134].to = 101; arcs[134].capacity = 10000; arcs[134].cost = 20;
	arcs[135].from = 20; arcs[135].to = 101; arcs[135].capacity = 10000; arcs[135].cost = 40;
	arcs[136].from = 25; arcs[136].to = 101; arcs[136].capacity = 10000; arcs[136].cost = 80;
	arcs[137].from = 30; arcs[137].to = 101; arcs[137].capacity = 10000; arcs[137].cost = 160;
	arcs[138].from = 35; arcs[138].to = 101; arcs[138].capacity = 10000; arcs[138].cost = 320;
	arcs[139].from = 40; arcs[139].to = 101; arcs[139].capacity = 10000; arcs[139].cost = 640;
	arcs[140].from = 45; arcs[140].to = 101; arcs[140].capacity = 10000; arcs[140].cost = 1280;
	arcs[141].from = 50; arcs[141].to = 101; arcs[141].capacity = 10000; arcs[141].cost = 2560;
	arcs[142].from = 55; arcs[142].to = 101; arcs[142].capacity = 10000; arcs[142].cost = 5120;
	arcs[143].from = 60; arcs[143].to = 101; arcs[143].capacity = 10000; arcs[143].cost = 10240;
	arcs[144].from = 65; arcs[144].to = 101; arcs[144].capacity = 10000; arcs[144].cost = 20480;
	arcs[145].from = 70; arcs[145].to = 101; arcs[145].capacity = 10000; arcs[145].cost = 40960;
	arcs[146].from = 75; arcs[146].to = 101; arcs[146].capacity = 10000; arcs[146].cost = 81920;
	arcs[147].from = 80; arcs[147].to = 101; arcs[147].capacity = 10000; arcs[147].cost = 163840;
	arcs[148].from = 85; arcs[148].to = 101; arcs[148].capacity = 10000; arcs[148].cost = 327680;
	arcs[149].from = 90; arcs[149].to = 101; arcs[149].capacity = 10000; arcs[149].cost = 655360;
	arcs[150].from = 95; arcs[150].to = 101; arcs[150].capacity = 10000; arcs[150].cost = 1310720;
	arcs[151].from = 100; arcs[151].to = 101; arcs[151].capacity = 10000; arcs[151].cost = 2621440;

	int node[102];

	for(int i = 0; i < 102; i++)
	{
		node[i] = i;
	}

	vector<Arc> gx;

	for (int i = 0; i < 151; i++)
	{
		gx.push_back(arcs[i]);
	}


	int x[152] = { 0 }, pi[102] = { 0 }, b[102] = { 0 };
	b[0] = 350;
	b[101] = -350;
	int e[102] = {0};
	
	for (int i = 0; i < 102; i++)
	{
		e[i] = b[i];
	}

	vector<int> E, D;


	for (int i = 0; i < 102; i++)
	{
		if (e[i] > 0)
			E.push_back(i);
		else if (e[i] < 0)
			D.push_back(i);
	}


	while (E.empty() == false)
	{

		std::random_device random_device;
		std::mt19937 engine{ random_device() };
		std::uniform_int_distribution<int> dist(0, E.size() - 1);
		std::uniform_int_distribution<int> dist(0, D.size() - 1);

		int k = E[dist(engine)];
		int l = D[dist(engine)];

		vector<int> perm, temp;
		int d[102] = { 10000 };
		d[k] = 0;
		int min;

		for (int i = 0; i < 102; i++)
		{
			temp.push_back(node[i]);
		}

		
	}

	
	char c;
	cout << "Press e to exit..." << endl;
	cin >> c;
	return 0;
}