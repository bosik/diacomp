package org.bosik.diacomp.core.entities.tech;

public class SearchResult
{
	private String	id;
	private String	name;

	public SearchResult(String id, String name)
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
