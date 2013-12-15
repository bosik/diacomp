package org.bosik.compensation.bo.basic;

public interface Named
{
	// private String name;

	// public Named(String name)
	// {
	// setName(name);
	// }

	String getName();

	// ================================ GET / SET ================================

	// public String getName()
	// {
	// return name;
	// }
	//
	// public void setName(String name)
	// {
	// if (name == null)
	// {
	// throw new NullPointerException("Name can't be null");
	// }
	// if (name.trim().equals(""))
	// {
	// throw new IllegalArgumentException("Name must contain non-whitespace characters");
	// }
	//
	// this.name = name;
	// }

	// ================================ CLONE ================================

	// @Override
	// public Named clone() throws CloneNotSupportedException
	// {
	// Named result = (Named) super.clone();
	// result.name = name;
	// return result;
	// }
}