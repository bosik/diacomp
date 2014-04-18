package org.bosik.diacomp.foodbase;

import java.io.Serializable;

public class Food implements Serializable
{
	private String	id;
	private String	name;

	public Food(String id, String name)
	{
		setId(id);
		setName(name);
	}

	public String getId()
	{
		return id;
	}

	public void setId(String id)
	{
		this.id = id;
	}

	public String getName()
	{
		return name;
	}

	public void setName(String name)
	{
		this.name = name;
	}
}
