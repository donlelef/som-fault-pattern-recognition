% This script cumputes a bidimensional gaussian distribution and plots it.
% This probability function is assumed to represent the probability of a fault
% to happen on the chip in the coordinates (x1, x2).
% After that, a map is created where random faults are simulated. The value
% 0 in the map means 'no fault' and 1 means 'fault', whereas -1 indicates 
% the points out of the circular wafer.

clear all
close all
clc

% Initializing benchmark parameters
ray = 30;
maximimumFaultProbabilities = 0.1:0.01:1;
testPerPattern = 5;
patterns = 1; % TO BE IMPROVED other pattern needed
testNumber = testPerPattern * patterns;
KDEpoints = 64;

for i = 1:length(maximimumFaultProbabilities)
    maximumFaultProbability = maximimumFaultProbabilities(i);
    for j = 1:testNumber
        % Computing multivariate normal probability density function
        varX = 1;
        varY = 1;
        Z = GaussianDensity(ray, varX, varY);
        
        % Filling sqare matrix with simbolic values. Fault are deloyed according to
        % the probabilit function.
        faultMap = fillSquareGrid(Z, 2*ray, maximumFaultProbability);
        faultMap = createCircularGrid(faultMap, ray);
        faultNumber(j) = length(find(faultMap==1));
        
        % KDE
        % call the routine, which has been saved in the current directory
        [faultX, faultY] = find(faultMap==1); % find seach elements by columns
        [bandwidth,extimatedDensity,X,Y] = kde2d([faultY,faultX], KDEpoints);
        
        % BENCHMARK
        % Computing and saving the average square error, ie the difference
        % between the value of the extimeted function and the value of the real
        % one.
        mu = [ray, ray]; 
        sigma = [varX, 0; 0, varY];
        trueDensity = mvnpdf([X(:), Y(:)], mu, sigma);
        trueDensity = reshape(trueDensity, length(X), length(Y));
        errorMatrix = (trueDensity - extimatedDensity).^2;
        error(j) = sum(sum(errorMatrix))/numel(errorMatrix);
    end
    meanGaussianError(i,1) = mean(error);
    meanGaussianFaults(i,1) = mean(faultNumber);
    
end
curve = fit(meanGaussianFaults, meanGaussianError, 'poly5', 'Normalize', 'on');
figure(1)
hold on
scatter(meanGaussianFaults, meanGaussianError, 'x')
plot(curve)
title('Mean error vs fault number')
legend('Simulated data', '5th degree fitting')
