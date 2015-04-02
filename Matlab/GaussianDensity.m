function Z = GaussianDensity(ray, varX, varY)
%GAUSSIANDENSITY return a gaussian desity matrix with specified variance

% Initializing variables for gussian plot
x1 = 0:1:2*ray;
x2 = x1;

mean = [ray,ray];
normalizedVariance = [varX, 0; 0, varY];
variance = ray.*normalizedVariance;

[X1,X2] = meshgrid(x1,x2);

rng = 'default'; % for reproducibility

% Computing and plotting multivariate normal probability density function
Z = mvnpdf([X1(:), X2(:)], mean, variance);
Z = reshape(Z, length(x1), length(x2));
end

