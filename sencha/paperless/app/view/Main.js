Ext.define('PaperlessPi.view.Main', {
    extend: 'Ext.tab.Panel',
    xtype: 'main',
    requires: [
        'Ext.TitleBar',
        'Ext.Video'
    ],
    config: {
        tabBarPosition: 'bottom',

        items: [
            {
                title: 'Scanner',
                iconCls: 'download',

                styleHtmlContent: true,
                scrollable: true,

                items: {
                    docked: 'top',
                    xtype: 'titlebar',
                    title: 'Scanner'
                }
            },
            {
                title: 'Images',
                iconCls: 'compose',

                items: [
                    {
                        docked: 'top',
                        xtype: 'titlebar',
                        title: 'Images'
                    }
                ]
            }
        ]
    }
});
