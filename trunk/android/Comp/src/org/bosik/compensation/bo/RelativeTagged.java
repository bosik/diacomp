package org.bosik.compensation.bo;

public interface RelativeTagged
{
	String getName();

	double getRelProts();

	double getRelFats();

	double getRelCarbs();

	double getRelValue();

	int getTag();

	void setTag(int tag);
}