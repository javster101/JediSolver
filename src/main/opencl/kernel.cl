kernel void brute(constant uchar* permutations, global const float2* points, global float* result,global short* ids, int const size) {

  local float distances [120];
  local float fastDist [36];
    const int pointsID = get_group_id(0);
    const int endOrStart = get_group_id(1);
    const int permuteID = get_local_id(0);
    const int firstOrSecond = get_group_id(2);
    if(pointsID<size && endOrStart < 6 && permuteID < 120 && firstOrSecond < 2){
        float2 point1 = points[pointsID * 12 + permutations[endOrStart*120*5 + permuteID*5] + firstOrSecond*6];
        float2 point2 = points[pointsID * 12 + permutations[endOrStart*120*5 + permuteID*5+1] + firstOrSecond*6];
        float2 point3 = points[pointsID * 12 + permutations[endOrStart*120*5 + permuteID*5+2] + firstOrSecond*6];
        float2 point4 = points[pointsID * 12 + permutations[endOrStart*120*5 + permuteID*5+3] + firstOrSecond*6];
        float2 point5 = points[pointsID * 12 + permutations[endOrStart*120*5 + permuteID*5+4] + firstOrSecond*6];
        float2 point6 = points[pointsID * 12 + endOrStart + firstOrSecond*6];
        float distance1 = fast_distance(point1,(float2)(5.15,-5)) * (1-firstOrSecond);
        distance1 += fast_distance(point1,point2); distance1 += fast_distance(point2,point3);
        distance1 += fast_distance(point3,point4); distance1 += fast_distance(point4,point5);
        distance1 += fast_distance(point5,point6);
        distances[permuteID] = distance1;
    }
    
    barrier(CLK_LOCAL_MEM_FENCE);
    if(permuteID == 0){
        float minDist1 = 10000000;
        float finalDist = 0;
        uchar minindex = 0;
        for(int i=0;i<120;i++){
            if(distances[i] < minDist1){
                minDist1 = distances[i];
                minindex = i;
            }
        }
        result[pointsID*12 + firstOrSecond*6 + endOrStart] = minDist1;
        ids[pointsID*12 + firstOrSecond*6 + endOrStart] = endOrStart*120*5+minindex*5;
    }
}
