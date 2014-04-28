package org.bosik.diacomp.core.services.search;

public class TagInfo
{
	private String	id;
	private int		tag;

	public TagInfo(String id, int tag)
	{
		this.id = id;
		this.tag = tag;
	}

	public String getId()
	{
		return id;
	}

	public void setId(String id)
	{
		this.id = id;
	}

	public int getTag()
	{
		return tag;
	}

	public void setTag(int tag)
	{
		this.tag = tag;
	}
}
