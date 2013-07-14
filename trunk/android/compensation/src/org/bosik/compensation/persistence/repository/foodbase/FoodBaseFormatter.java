package org.bosik.compensation.persistence.repository.foodbase;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import org.bosik.compensation.persistence.entity.foodbase.Food;
import org.bosik.compensation.persistence.entity.foodbase.FoodBase;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class FoodBaseFormatter
{
	private static Document getDOM(String xmlData)
	{
		try
		{
			InputStream is = new ByteArrayInputStream(xmlData.getBytes("UTF-8"));
			DocumentBuilderFactory DBF = DocumentBuilderFactory.newInstance();
			DocumentBuilder DB = DBF.newDocumentBuilder();
			return DB.parse(is);
		} catch (UnsupportedEncodingException e)
		{
			e.printStackTrace();
		} catch (ParserConfigurationException e)
		{
			e.printStackTrace();
		} catch (SAXException e)
		{
			e.printStackTrace();
		} catch (IOException e)
		{
			e.printStackTrace();
		}

		return null;
	}

	public static int getVersion(String xmlData)
	{
		Document dom = getDOM(xmlData);
		Element root = dom.getDocumentElement();
		return Integer.parseInt(root.getAttribute("version"));
	}

	public static FoodBase read(String xmlData)
	{
		Document dom = getDOM(xmlData);
		Element root = dom.getDocumentElement();
		NodeList nodes = root.getChildNodes();

		FoodBase foodBase = new FoodBase();
		foodBase.beginUpdate();
		foodBase.setVersion(Integer.parseInt(root.getAttribute("version")));

		for (int i = 0; i < nodes.getLength(); i++)
		{
			Node node = nodes.item(i);

			if (node instanceof Element)
			{
				Element x = (Element) node;

				Food food = new Food();
				food.setName(x.getAttribute("name"));
				food.setRelProts(Double.parseDouble(x.getAttribute("prots")));
				food.setRelFats(Double.parseDouble(x.getAttribute("fats")));
				food.setRelCarbs(Double.parseDouble(x.getAttribute("carbs")));
				food.setRelValue(Double.parseDouble(x.getAttribute("val")));
				food.setFromTable(x.getAttribute("table").equals("True"));

				foodBase.add(food);
			}
		}

		foodBase.endUpdate();
		return foodBase;
	}

	public static String write(FoodBase foodBase)
	{
		// TODO: implement
		return null;
	}
}
