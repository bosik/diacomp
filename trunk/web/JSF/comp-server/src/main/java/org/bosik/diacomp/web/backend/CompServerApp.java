package org.bosik.diacomp.web.backend;

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Application;

@ApplicationPath("/api/")
public class CompServerApp extends Application
{
	//	@Override
	//	public Set<Class<?>> getClasses()
	//	{
	//		final Set<Class<?>> classes = new HashSet<Class<?>>();
	//		// register root resource
	//		classes.add(AuthRestService.class);
	//		classes.add(DiaryRestService.class);
	//		//		classes.add(DishbaseRestService.class);
	//		//		classes.add(FoodbaseRestService.class);
	//		classes.add(SystemRestService.class);
	//		return classes;
	//	}
}
