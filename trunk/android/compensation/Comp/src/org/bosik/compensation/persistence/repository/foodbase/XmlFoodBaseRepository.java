package org.bosik.compensation.persistence.repository.foodbase;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import org.bosik.compensation.persistence.entity.foodbase.FoodBase;

public class XmlFoodBaseRepository implements FoodBaseRepository
{
	private String fileName;

	public XmlFoodBaseRepository(String fileName)
	{
		this.fileName = fileName;
	}

	private String getFileContent(FileInputStream fis)
	{
		try
		{
			StringBuilder sb = new StringBuilder();
			Reader r = new InputStreamReader(fis, "UTF-8"); // or whatever encoding
			int ch = r.read();
			while (ch >= 0)
			{
				sb.append(ch);
				ch = r.read();
			}
			return sb.toString();
		} catch (UnsupportedEncodingException e)
		{
			e.printStackTrace();
		} catch (IOException e)
		{
			e.printStackTrace();
		}
		return null;
	}

	@Override
	public int getVersion()
	{
		try
		{
			return FoodBaseFormatter.getVersion(getFileContent(new FileInputStream(fileName)));
		} catch (FileNotFoundException e)
		{
			return 0;
		}
	}

	@Override
	public FoodBase getBase()
	{
		try
		{
			return FoodBaseFormatter.read(getFileContent(new FileInputStream(fileName)));
		} catch (FileNotFoundException e)
		{
			// TODO: or just null?
			return new FoodBase();
		}
	}

	@Override
	public void postBase(FoodBase base)
	{
		// TODO Auto-generated method stub

	}

}
