package org.bosik.compensation.bo;

import org.bosik.compensation.bo.basic.Named;

public interface RelativeTagged extends Named
{
	public int getTag();

	public void setTag(int tag);

	double getRelProts();

	double getRelFats();

	double getRelCarbs();

	double getRelValue();
}