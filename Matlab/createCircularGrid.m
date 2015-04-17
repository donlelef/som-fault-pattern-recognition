function faultMap = createCircularGrid(faultMap, ray, outValue)
% Creates a circular grid from a square one by inserting the value -1 in
% every pixel which is not whitin the circle of ray "ray".
    
    pixelNumber = length(faultMap);
    for i=1:pixelNumber
        for j=1:pixelNumber
            if ((i-ray)^2 + (j-ray)^2 > ray^2)
                faultMap(i,j)=outValue;
            end
        end
    end
end

