package org.bosik.compensation.persistence.repository.common;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;

/**
 * Предоставляет необходимые поля для работы с xml, а также статический метод, проводящий парсинг
 * xml-строки.
 * 
 * @author Bosik
 * 
 */
public class XmlFormatter
{
	protected static DocumentBuilder builder;
	protected static Transformer transformer;

	static
	{
		try
		{
			builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
		} catch (ParserConfigurationException e)
		{
			e.printStackTrace();
		}

		try
		{
			transformer = TransformerFactory.newInstance().newTransformer();
		} catch (TransformerConfigurationException e)
		{
			e.printStackTrace();
		}
	}

	protected static Document getDocument(String xmlData)
	{
		try
		{
			InputStream stream = new ByteArrayInputStream(xmlData.getBytes("UTF-8"));
			return builder.parse(stream);
		} catch (UnsupportedEncodingException e)
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
}