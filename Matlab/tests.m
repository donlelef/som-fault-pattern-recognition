% Nothing useful here!
clc
clear all
close all

x = [1,2,3];
y = x;
[X, Y] = meshgrid(x,y);

a = mvnpdf(0,0)