% Fill a square grid with 0s or 1s according to the probabilityFuncion 
% The higher is the value probabilityFunction takes in a cetrain point,the
% higher is the probability that pixel is set to 1.

function faultMap = fillSquareGrid(probabilityFunction, pixelNumber, maxFaultProbability)
    
    maxZ = max(max(probabilityFunction));
    faultMap = zeros(pixelNumber, pixelNumber);
    rng = 'default';

    for i=1:pixelNumber
        for j=1:pixelNumber
        
            if ((rand) < maxFaultProbability*probabilityFunction(i,j)/maxZ)
                faultMap(i,j) = 1;
            else
                faultMap(i,j) = 0;
            end
        end
    end
end

